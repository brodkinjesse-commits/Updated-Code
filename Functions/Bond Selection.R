# ==============================================================================
# BOND SELECTION: the cheapest self-funding bond ladder
# ==============================================================================
# Picks the bond portfolio that secures `ladder_years` of the retiree's income
# for the least money spent at t0.
#
#   minimise   sum_j x_j                      (rands invested in each bond)
#   subject to (1) the cash account never goes negative in any month
#              (2) no bond redeems after the ladder ends
#              x_j >= 0                       (long only)
#
# Month 1 is handled by holding exactly one monthly instalment in cash at t0.
# The retiree draws at t0 but the earliest any bond can pay is a month later, so
# that first instalment can never come from a bond - it is set aside as cash
# rather than being made the optimizer's problem.
#
# The income being funded is NOT level: `Inflation_pa` (annual %, default 5.7)
# escalates the withdrawal every month, so the ladder is sized against a rising
# income rather than a flat one. Setting Inflation_pa = 0 recovers the old
# level-income ladder exactly.
#
# Argument-order note: Inflation_pa sits BEFORE ladder_years in every signature
# below, matching the agreed public API. It has a default and ladder_years does
# not, so positional calls must still pass it - optimize_bond_ladder(pot, wd,
# ladder_years) would read ladder_years as the inflation rate. Pass ladder_years
# by name if in doubt.
#
# There is no PV/duration/convexity/PV01 matching here. Matching cash flows
# month by month is a stronger requirement than matching sensitivities: if the
# money is there when it is needed, the ladder does its job regardless of what
# the curve does. Constraint (2) also means the ladder self-liquidates - every
# bond it holds pays out inside the ladder window, so there is nothing left to
# mark to market and sell at the end.
# ==============================================================================

if (!require("lpSolve"))   install.packages("lpSolve")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl"))    install.packages("readxl")
library(lpSolve)
library(lubridate)
library(readxl)

MTM_FILE <- file.path("Data", "Bond Data.xls")

# ------------------------------------------------------------------------------
# 1. BOND UNIVERSE (from the MTM extract)
# ------------------------------------------------------------------------------
mtm_raw <- read_excel(MTM_FILE, sheet = "MTM", skip = 5)
mtm_raw <- as.data.frame(mtm_raw[!is.na(mtm_raw$`Bond Code`), ])

# Time zero for the whole model: 31 July 2026. This is the date the ladder is
# bought, the date the first withdrawal is paid, and the origin every t_years
# and month_index is measured from. It is read from the MTM extract's own
# valuation date (cell C4) so bond prices and t0 can never drift apart, and
# checked against the intended date rather than hardcoded silently - if a newer
# extract is dropped in, this stops loudly instead of quietly repricing the
# ladder off a different day.
MODEL_START_DATE <- as.Date("2026-07-31")

valuation_date <- as.Date(suppressMessages(read_excel(MTM_FILE, sheet = "MTM", range = "C4",
                                                      col_names = FALSE))[[1]][1])

if (!identical(valuation_date, MODEL_START_DATE)) {
  stop(sprintf(paste0("MTM valuation date (%s) is not the model start date (%s). ",
                      "Bond prices and time zero must be the same day - either supply the ",
                      "matching MTM extract or update MODEL_START_DATE deliberately."),
               format(valuation_date), format(MODEL_START_DATE)))
}

bonds_fixed <- data.frame(
  bond_code             = mtm_raw$`Bond Code`,
  coupon_rate_nominal   = as.numeric(mtm_raw$Coupon),          # % p.a., paid semi-annually
  redemption_date       = as.Date(mtm_raw$`Maturity`),
  redemption_amount_pct = 100,
  market_price          = as.numeric(mtm_raw$`All in price`),  # dirty price = actual cash outlay to buy
  clean_price           = as.numeric(mtm_raw$`Clean Price`),
  stringsAsFactors      = FALSE
)
bonds_fixed$years_to_maturity <- as.numeric(bonds_fixed$redemption_date - valuation_date) / 365.25

# ------------------------------------------------------------------------------
# 2. YIELD CURVE
# ------------------------------------------------------------------------------
# PLACEHOLDER: flat 6% until a fitted term structure is wired in. Used to value
# bonds (calculate_bond_metrics) and, elsewhere in the model, the ARVA annuity
# factor. It does NOT drive the ladder choice - the optimizer works off actual
# cash flow dates and quoted prices, so the ladder is unaffected by this
# placeholder. The BEASSA read + interpolation is kept below so swapping in the
# real curve later is a small change.
#
# curve_raw <- read_excel(MTM_FILE, sheet = "BEASSA Yield Curve", skip = 6)
# names(curve_raw) <- c("t", "yield_pa")
# curve_raw <- curve_raw[!is.na(curve_raw$t), ]
# curve_raw <- curve_raw[order(curve_raw$t), ]
# curve_yield <- function(t_years) {
#   t_years <- pmin(pmax(t_years, min(curve_raw$t)), max(curve_raw$t))
#   approx(curve_raw$t, curve_raw$yield_pa, xout = t_years)$y
# }

curve_yield <- function(t_years) {
  rep(6, length(t_years))   # flat 6% p.a. placeholder
}

# ------------------------------------------------------------------------------
# 3. CASH FLOW -> SIMULATION MONTH MAPPING
# ------------------------------------------------------------------------------
# Which simulation month a cash flow dated `dates` falls in, measured from time
# zero. Month m spans [t0 %m+% months(m - 1), t0 %m+% months(m)), so:
#   month 1 = 31 Jul 2026 -> 30 Aug 2026
#   month 2 = 31 Aug 2026 -> 29 Sep 2026   ... and so on.
#
# Month 1 belongs to the retiree alone: the ladder is bought at t0 and the first
# withdrawal is paid at t0, but the earliest a bond can pay anything is 31 Aug
# 2026, one full coupon month later. Any flow landing inside month 1 is
# therefore pushed to month 2 by the floor below - cash is credited no earlier
# than it can actually arrive.
#
# Whole months are counted by calendar arithmetic, not by rounding t_years * 12:
# rounding credited a flow up to ~2 weeks early and, with a 31 July origin, put
# an end-of-August coupon in month 1. The year/month difference is corrected by
# at most one step because %m+% clamps to month end (t0 + 1 month is 30 Aug, not
# the non-existent 31 Aug), which the raw difference cannot see.
month_index_from_date <- function(dates, valuation_date, min_month = 2L) {

  k <- 12L * (year(dates) - year(valuation_date)) + (month(dates) - month(valuation_date))

  too_far  <- valuation_date %m+% months(k) > dates
  k[too_far] <- k[too_far] - 1L
  too_near <- valuation_date %m+% months(k + 1L) <= dates
  k[too_near] <- k[too_near] + 1L

  pmax(as.integer(min_month), as.integer(k) + 1L)
}

# ------------------------------------------------------------------------------
# 4. ONE BOND'S CASH FLOWS (per 1 unit = R100 par)
# ------------------------------------------------------------------------------
# The coupon cycle is anchored to the bond's OWN redemption date (its real
# day-of-year), stepping backward in exact 6-month jumps until reaching
# valuation_date - NOT stepped forward from valuation_date, which would
# incorrectly force every bond's coupons onto valuation_date's day-of-year.
# Each date is computed directly from redemption_date (not chained from the
# previous one) using %m-%, so repeated month-end rollbacks cannot drift the day
# of month over many periods. The final coupon coincides with redemption_date,
# and redemption is added onto that same cash flow.
single_bond_cashflows <- function(coupon_rate_nominal, redemption_amount_pct,
                                  redemption_date, valuation_date) {

  k <- 0
  repeat {
    nxt <- redemption_date %m-% months(6 * (k + 1))
    if (nxt <= valuation_date) break
    k <- k + 1
  }
  dates <- sort(redemption_date %m-% months(seq(0, by = 6, length.out = k + 1)))
  dates <- dates[dates > valuation_date]

  coupon_amt     <- 100 * (coupon_rate_nominal / 100) / 2   # half the nominal rate, twice a year
  cf             <- rep(coupon_amt, length(dates))
  cf[length(cf)] <- cf[length(cf)] + 100 * (redemption_amount_pct / 100)

  type             <- rep("Coupon", length(dates))
  type[length(type)] <- "Coupon + Redemption"

  data.frame(date    = dates,
             t_years = as.numeric(difftime(dates, valuation_date, units = "days")) / 365.25,
             type    = type,
             amount  = cf,
             stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------------
# 5. BOND VALUATION (per 1 unit = R100 par)
# ------------------------------------------------------------------------------
# PV and Macaulay duration from each bond's own real cash flow dates, discounted
# at curve_yield(). `market_price` stays the quoted MTM price (what you actually
# pay); `pv` is an independent theoretical value, so the two can legitimately
# differ where curve_yield() does not match the market's implied yield.
#
# The optimizer does not use this - it buys at market_price and matches cash
# flows. It is here for reporting, and for marking a held ladder to model at a
# later date (mark_to_model_bond_value() in Dynamic_Ladder.R).
calculate_bond_metrics <- function(bonds_fixed, valuation_date) {

  n_bonds <- nrow(bonds_fixed)
  metrics <- data.frame(bond_code    = bonds_fixed$bond_code,
                        market_price = bonds_fixed$market_price,
                        pv           = numeric(n_bonds),
                        mac_duration = numeric(n_bonds),
                        stringsAsFactors = FALSE)

  for (i in seq_len(n_bonds)) {
    cf <- single_bond_cashflows(bonds_fixed$coupon_rate_nominal[i],
                                bonds_fixed$redemption_amount_pct[i],
                                bonds_fixed$redemption_date[i],
                                valuation_date)
    y    <- curve_yield(cf$t_years) / 100
    pv_t <- cf$amount / (1 + y)^cf$t_years

    metrics$pv[i]           <- sum(pv_t)
    metrics$mac_duration[i] <- sum(cf$t_years * pv_t) / sum(pv_t)
  }

  metrics
}

# ------------------------------------------------------------------------------
# 6. LIABILITY: level monthly income, paid in advance
# ------------------------------------------------------------------------------
# The retiree is paid monthly, in advance: the first instalment is drawn
# immediately (t = 0) and the last at the start of the final month of the
# ladder. ladder_years can be fractional (a mid-year rebalance), so work in
# whole months throughout.
#
# The instalment grows with inflation month by month. `Inflation_pa` is an
# ANNUAL rate in percent, converted to the equivalent monthly rate
#     i_m = (1 + Inflation_pa/100)^(1/12) - 1
# so twelve monthly step-ups compound to exactly Inflation_pa over the year.
# Month 1 is paid unescalated (it is drawn today); month m is paid
#     W * (1 + i_m)^(m - 1).
# Stepping monthly rather than annually keeps this liability consistent with the
# CPI-indexed monthly payout the simulation actually makes.
calculate_liability_metrics <- function(total_pot_value, withdrawal_rate_pct,
                                       Inflation_pa = 5.7, ladder_years) {

  if (!is.finite(Inflation_pa) || Inflation_pa <= -100) {
    stop("Inflation_pa must be a finite annual rate in percent, greater than -100")
  }

  annual_withdrawal  <- total_pot_value * (withdrawal_rate_pct / 100)
  monthly_withdrawal <- annual_withdrawal / 12
  n_months           <- max(1L, as.integer(round(ladder_years * 12)))

  i_m   <- (1 + Inflation_pa / 100)^(1 / 12) - 1
  t_vec <- (seq_len(n_months) - 1) / 12          # annuity-due: t = 0, 1/12, ...
  w_vec <- monthly_withdrawal * (1 + i_m)^(seq_len(n_months) - 1)
  y_vec <- curve_yield(t_vec) / 100
  pv_t  <- w_vec * (1 + y_vec)^(-t_vec)

  list(annual_withdrawal   = annual_withdrawal,
       monthly_withdrawal  = monthly_withdrawal,   # month 1, before any escalation
       monthly_withdrawals = w_vec,                # the full escalating schedule
       inflation_pa        = Inflation_pa,
       monthly_inflation   = i_m,
       n_months            = n_months,
       total_withdrawn     = sum(w_vec),
       pv                  = sum(pv_t),
       mac_duration        = sum(t_vec * pv_t) / sum(pv_t))
}

# ------------------------------------------------------------------------------
# 7. LADDER CASH FLOW SCHEDULE: bond inflows less withdrawals, month by month
# ------------------------------------------------------------------------------
# Timing conventions, shared with the optimizer:
#   - Bond flows are bucketed by month_index_from_date(), floored at month 2.
#   - Withdrawals are paid at the START of each month, m = 1..n_months.
#
# Because coupons are lumpy (semi-annual) and withdrawals are monthly,
# net_cashflow is negative in most months and sharply positive in coupon and
# redemption months. Withdrawals escalate at Inflation_pa (same monthly
# conversion as calculate_liability_metrics()), so `annual_withdrawal` fixes the
# STARTING rate of income, not the amount paid in every month. `cum_net` is the running total on zero starting cash;
# `balance` adds the starting cash the ladder is actually bought with, which by
# construction is one monthly instalment. Month 1's cum_net is always exactly
# minus one instalment, so balance starts at 0 and must stay >= 0 thereafter.
#
# `starting_cash` is an argument only so a different assumption can be tested;
# optimize_bond_ladder() always uses the default.
ladder_cashflow_schedule <- function(bond_units,
                                     annual_withdrawal,
                                     Inflation_pa   = 5.7,
                                     ladder_years,
                                     bonds          = bonds_fixed,
                                     valuation_date = NULL,
                                     starting_cash  = annual_withdrawal / 12) {

  if (is.null(valuation_date)) valuation_date <- get("valuation_date", envir = .GlobalEnv)

  # bond_units is matched to bonds BY NAME, so an unnamed vector or a typo in a
  # bond code silently finds nothing. Both used to surface much later as
  # "argument is of length zero", which says nothing about the real cause.
  if (length(bond_units) > 0) {
    if (is.null(names(bond_units))) {
      stop("bond_units must be a NAMED vector of bond codes, e.g. c(R2033 = 66666.67). ",
           "Holdings are matched to bonds by name, not by position.")
    }
    unknown <- setdiff(names(bond_units)[bond_units > 0], bonds$bond_code)
    if (length(unknown)) {
      stop("unknown bond code(s): ", paste(unknown, collapse = ", "),
           ". Valid codes: ", paste(bonds$bond_code, collapse = ", "))
    }
  }

  if (!is.finite(Inflation_pa) || Inflation_pa <= -100) {
    stop("Inflation_pa must be a finite annual rate in percent, greater than -100")
  }

  n_months           <- max(1L, as.integer(round(ladder_years * 12)))
  monthly_withdrawal <- annual_withdrawal / 12
  months             <- seq_len(n_months)

  # the escalating instalments; month 1 is paid unescalated
  i_m                 <- (1 + Inflation_pa / 100)^(1 / 12) - 1
  monthly_withdrawals <- monthly_withdrawal * (1 + i_m)^(months - 1)

  held <- bond_units[!is.na(bond_units) & bond_units > 0]

  bond_flows <- if (length(held) == 0) {
    data.frame(bond_code = character(0), date = as.Date(character(0)),
               month_index = integer(0), type = character(0), amount = numeric(0),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(names(held), function(bcode) {
      b  <- bonds[bonds$bond_code == bcode, ]
      cf <- single_bond_cashflows(b$coupon_rate_nominal, b$redemption_amount_pct,
                                  b$redemption_date, valuation_date)
      data.frame(bond_code   = bcode,
                 date        = cf$date,
                 month_index = month_index_from_date(cf$date, valuation_date),
                 type        = cf$type,
                 amount      = cf$amount * held[[bcode]],   # per R100 par -> units held
                 stringsAsFactors = FALSE)
    }))
  }

  if (nrow(bond_flows)) bond_flows <- bond_flows[order(bond_flows$date, bond_flows$bond_code), ]
  in_ladder <- if (nrow(bond_flows)) bond_flows$month_index <= n_months else logical(0)

  bucket <- function(rows) {
    if (!any(rows)) return(rep(0, n_months))
    as.numeric(tapply(bond_flows$amount[rows],
                      factor(bond_flows$month_index[rows], levels = months),
                      sum, default = 0))
  }
  is_redemption <- if (nrow(bond_flows)) grepl("Redemption", bond_flows$type, fixed = TRUE) else logical(0)

  schedule <- data.frame(
    month         = months,
    month_start   = valuation_date %m+% months(months - 1L),
    bond_inflow   = bucket(in_ladder),
    coupon_in     = bucket(in_ladder & !is_redemption),
    redemption_in = bucket(in_ladder & is_redemption),
    withdrawal    = monthly_withdrawals,
    stringsAsFactors = FALSE
  )
  schedule$net_cashflow <- schedule$bond_inflow - schedule$withdrawal
  schedule$cum_net      <- cumsum(schedule$net_cashflow)
  schedule$balance      <- starting_cash + schedule$cum_net

  list(schedule           = schedule,
       bond_flows         = bond_flows[in_ladder, ],
       overhang           = bond_flows[!in_ladder, ],
       n_months            = n_months,
       starting_cash       = starting_cash,
       monthly_withdrawal  = monthly_withdrawal,
       monthly_withdrawals = monthly_withdrawals,
       inflation_pa        = Inflation_pa,
       monthly_inflation   = i_m,
       total_inflow       = sum(schedule$bond_inflow),
       total_withdrawn    = sum(schedule$withdrawal),
       overhang_value     = if (nrow(bond_flows)) sum(bond_flows$amount[!in_ladder]) else 0,
       min_balance        = min(schedule$balance))
}

# Turn a weight vector (proportions of the bond sleeve) into units held, for
# testing a hand-picked ladder against the optimizer's answer:
#     units_j = weight_j * sleeve_value / market_price_j
bond_units_from_weights <- function(weights, sleeve_value, bonds = bonds_fixed) {

  if (is.null(names(weights))) stop("weights must be a named vector of bond codes")
  unknown <- setdiff(names(weights), bonds$bond_code)
  if (length(unknown)) stop("unknown bond code(s): ", paste(unknown, collapse = ", "))
  if (any(weights < 0)) stop("weights must be non-negative (the model is long-only)")

  out <- setNames(rep(0, nrow(bonds)), bonds$bond_code)
  out[names(weights)] <- weights
  out * sleeve_value / bonds$market_price
}

# ------------------------------------------------------------------------------
# 8. THE OPTIMIZER
# ------------------------------------------------------------------------------
# Cheapest bond portfolio that funds `ladder_years` of income.
#
#   variables   x_j >= 0, the RANDS INVESTED in bond j (units_j = x_j / price_j)
#   minimise    sum_j x_j
#   subject to  balance_m >= 0 for every month m
#               only bonds redeeming inside the ladder are eligible
#
# The balance constraint written out. The ladder starts with exactly one monthly
# instalment in cash (C0 = W_1), pays the escalating instalment W_m at the start
# of each month m, and receives bond flows as they land:
#
#   balance_m = W_1 + sum_j x_j * CumIn[m, j] / price_j - sum_{k<=m} W_k  >= 0
#             =>  sum_j x_j * CumIn[m, j] / price_j  >=  CumW_m - W_1
#
# where CumIn[m, j] is bond j's cumulative cash flow per R100 par through month
# m, and CumW_m is the cumulative income paid through month m. With no inflation
# the right-hand side collapses to (m - 1) * W, the level-income case. At m = 1
# it reads 0 >= 0 - true for any portfolio, which is exactly the "month 1 is
# covered by cash" rule - so the LP only carries months 2..n.
#
# Escalation makes the ladder strictly more expensive, and disproportionately so
# at the long end: the last month of a 10-year ladder at 5.7% needs 1.057^9.92
# ~= 1.73x the first month's income, so the longer maturities have to fund much
# more than a level liability would ask of them.
#
# The bond universe (bonds_fixed) is taken from this file. So is t0, unless a
# `valuation_date` is passed: the dynamic simulation rebalances the ladder
# part-way through retirement, and every date-dependent piece of the solve -
# which bonds are still alive, which are short enough to redeem inside the
# remaining ladder, and when each coupon lands - has to be measured from the
# rebalance date, not from t0. Left NULL (the thesis script's t0 call) it
# falls back to this file's own valuation_date, so the default is unchanged.
#
# Returns a list:
#   total_cost       rands spent on bonds
#   cash_at_start    the month-1 instalment held in cash (= annual/12)
#   total_outlay     total_cost + cash_at_start, the full t0 cost of the ladder
#   bond_units       named vector aligned to bonds_fixed (0 for bonds not held),
#                    ready to pass to ladder_cashflow_schedule()
#   bond_amounts     the same in rands invested
#   allocation       one row per bond actually held
#   liability        calculate_liability_metrics() output
#   schedule         the realised ladder_cashflow_schedule()
#   min_balance      tightest cash balance over the ladder (should be ~0)
#   n_months, ladder_years, inflation_pa, eligible_bonds
optimize_bond_ladder <- function(total_pot_value, withdrawal_rate_pct, Inflation_pa = 5.7,
                                 ladder_years, valuation_date = NULL) {

  if (is.null(valuation_date)) valuation_date <- get("valuation_date", envir = .GlobalEnv)

  liability <- calculate_liability_metrics(total_pot_value, withdrawal_rate_pct,
                                          Inflation_pa = Inflation_pa,
                                          ladder_years = ladder_years)
  n_months  <- liability$n_months
  W         <- liability$monthly_withdrawal      # month 1's instalment, held in cash
  w_vec     <- liability$monthly_withdrawals     # the escalating schedule

  if (!is.finite(W) || W <= 0) stop("liability is zero, negative or non-finite - nothing to fund")
  if (n_months < 2) stop("ladder is shorter than 2 months - month 1 is covered by cash, so there is nothing to solve")

  # --- eligibility: priced, not already matured, redeems inside the ladder ---
  # month_index_from_date() floors at month 2, so a bond that has already
  # matured would otherwise look eligible - the redemption_date test excludes it.
  eligible <- is.finite(bonds_fixed$market_price) & bonds_fixed$market_price > 0 &
              bonds_fixed$redemption_date > valuation_date &
              month_index_from_date(bonds_fixed$redemption_date, valuation_date) <= n_months

  if (!any(eligible)) {
    stop(sprintf(paste0("no eligible bonds: none of the %d bonds in the universe redeems inside a ",
                        "%.2f-year (%d-month) ladder. Shortest available maturity is %.2f years."),
                 nrow(bonds_fixed), ladder_years, n_months,
                 min(as.numeric(bonds_fixed$redemption_date[bonds_fixed$redemption_date > valuation_date] -
                                valuation_date)) / 365.25))
  }

  elig  <- bonds_fixed[eligible, , drop = FALSE]
  codes <- elig$bond_code
  n_b   <- nrow(elig)

  # --- cumulative inflow per RAND invested, by month ------------------------
  # Bucketing is month_index_from_date(), identical to ladder_cashflow_schedule(),
  # so what the LP constrains is exactly the balance reported back.
  cum_in <- matrix(0, nrow = n_months, ncol = n_b, dimnames = list(NULL, codes))
  for (j in seq_len(n_b)) {
    cf <- single_bond_cashflows(elig$coupon_rate_nominal[j], elig$redemption_amount_pct[j],
                                elig$redemption_date[j], valuation_date)
    mi <- month_index_from_date(cf$date, valuation_date)
    cum_in[, j] <- cumsum(as.numeric(tapply(cf$amount, factor(mi, levels = seq_len(n_months)),
                                            sum, default = 0))) / elig$market_price[j]
  }

  # --- the LP ---------------------------------------------------------------
  # Months 2..n only (month 1 reads 0 >= 0). Each row is divided by its own
  # right-hand side and x is expressed in units of the annual withdrawal, so
  # every coefficient sits near 1 - without that, rand amounts (~1e7) and
  # per-rand cash flow coefficients (~1e0) span seven orders of magnitude and
  # lpSolve's default tolerances start to bite.
  # rhs_m = cumulative income paid through month m, less the month-1 instalment
  # already sitting in cash. Strictly increasing and > 0 for every m >= 2, so
  # scaling each row by its own rhs is always safe.
  m_rows <- 2:n_months
  rhs    <- cumsum(w_vec)[m_rows] - W
  scale  <- liability$annual_withdrawal

  # A month that NO eligible bond can reach is unsatisfiable however much is
  # spent, and is by far the most common reason this fails: the cash buffer only
  # covers month 1, so something must pay by month 2. Diagnose it explicitly -
  # lpSolve would otherwise just report "infeasible" with no clue why.
  dead <- m_rows[apply(cum_in[m_rows, , drop = FALSE], 1, function(r) all(r <= 0))]
  if (length(dead)) {
    # cum_in is cumulative, so the first month with any positive entry is the
    # earliest month any eligible bond pays anything.
    first_pay <- min(which(apply(cum_in, 1, function(r) any(r > 0))))
    stop(sprintf(paste0("infeasible: month %d (%s) has no bond cash flow to draw on, and only ",
                        "month 1 is covered by cash. The earliest any eligible bond pays is ",
                        "month %d. Months %s are unreachable.\n",
                        "  Fix: lengthen the ladder (widens the eligible universe), or add ",
                        "shorter-dated paper / T-bills, or hold cash for the first %d months."),
                 dead[1], format(valuation_date %m+% months(dead[1] - 1L)),
                 first_pay, paste(range(dead), collapse = "-"), max(dead)))
  }

  const_mat <- (cum_in[m_rows, , drop = FALSE] * scale) / rhs

  sol <- lp("min", rep(1, n_b), const_mat, rep(">=", length(m_rows)), rep(1, length(m_rows)))

  if (sol$status != 0) {
    stop(sprintf(paste0("infeasible: no combination of the %d eligible bond(s) can cover the ",
                        "withdrawals month by month. The ladder needs shorter-dated paper, a ",
                        "lower withdrawal rate, or a longer ladder (which widens the universe)."),
                 n_b))
  }

  # --- unpack, aligned to the FULL bond universe ----------------------------
  x_bonds <- sol$solution * scale
  x_bonds[x_bonds < 1e-8] <- 0        # scrub solver dust so ~R0 holdings do not show up

  bond_amounts <- setNames(rep(0, nrow(bonds_fixed)), bonds_fixed$bond_code)
  bond_units   <- bond_amounts
  bond_amounts[codes] <- x_bonds
  bond_units[codes]   <- x_bonds / elig$market_price

  total_cost <- sum(bond_amounts)

  allocation <- data.frame(bond_code         = codes,
                           redemption_date   = elig$redemption_date,
                           years_to_maturity = as.numeric(elig$redemption_date - valuation_date) / 365.25,
                           coupon_rate       = elig$coupon_rate_nominal,
                           market_price      = elig$market_price,
                           amount_invested   = as.numeric(x_bonds),
                           units             = as.numeric(x_bonds / elig$market_price),
                           weight            = if (total_cost > 0) as.numeric(x_bonds) / total_cost else 0,
                           stringsAsFactors  = FALSE)
  allocation <- allocation[allocation$units > 0, , drop = FALSE]
  allocation <- allocation[order(allocation$redemption_date), , drop = FALSE]
  rownames(allocation) <- NULL

  # --- verify against the schedule, rebuilt from scratch --------------------
  # Not trusting the LP matrix is the point: if the two ever disagree, the
  # bucketing conventions have drifted apart and this catches it.
  sched <- ladder_cashflow_schedule(bond_units, liability$annual_withdrawal,
                                    Inflation_pa   = Inflation_pa,
                                    ladder_years   = ladder_years,
                                    valuation_date = valuation_date)

  list(total_cost     = total_cost,
       cash_at_start  = W,
       total_outlay   = total_cost + W,
       bond_units     = bond_units,
       bond_amounts   = bond_amounts,
       allocation     = allocation,
       liability      = liability,
       schedule       = sched,
       min_balance    = sched$min_balance,
       n_months       = n_months,
       ladder_years   = ladder_years,
       inflation_pa   = Inflation_pa,
       eligible_bonds = codes)
}

# ------------------------------------------------------------------------------
# 9. CONSOLE REPORT
# ------------------------------------------------------------------------------
print_ladder_optimization <- function(res, digits = 2) {

  # zap solver dust so a binding constraint prints as 0.00, not -0.00
  fmt <- function(x) formatC(ifelse(abs(x) < 0.5 * 10^(-digits), 0, x),
                             format = "f", big.mark = ",", digits = digits)

  cat("\n=================================================================\n")
  cat(sprintf(" CHEAPEST SELF-FUNDING LADDER  |  %.2f years (%d months)\n",
              res$ladder_years, res$n_months))
  cat("=================================================================\n")
  cat(sprintf(" Income secured       : R %s p.a.  (R %s in month 1)\n",
              fmt(res$liability$annual_withdrawal), fmt(res$liability$monthly_withdrawal)))
  cat(sprintf(" Escalating at        : %.2f%% p.a.  (R %s by month %d)\n",
              res$liability$inflation_pa,
              fmt(res$liability$monthly_withdrawals[res$n_months]), res$n_months))
  cat(sprintf(" Total paid out       : R %s over the ladder\n",
              fmt(res$liability$total_withdrawn)))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf(" BOND PURCHASE COST   : R %s\n", fmt(res$total_cost)))
  cat(sprintf(" + cash for month 1   : R %s\n", fmt(res$cash_at_start)))
  cat(sprintf(" = TOTAL t0 OUTLAY    : R %s\n", fmt(res$total_outlay)))
  cat(sprintf(" Bonds used           : %d of %d eligible\n",
              nrow(res$allocation), length(res$eligible_bonds)))
  cat(sprintf(" Tightest balance     : R %s   (0 = constraint binds, i.e. cheapest)\n",
              fmt(res$min_balance)))
  cat(sprintf(" Left over at the end : R %s\n",
              fmt(res$schedule$schedule$balance[res$n_months])))
  cat("-----------------------------------------------------------------\n")

  if (nrow(res$allocation)) {
    print(data.frame(bond     = res$allocation$bond_code,
                     redeems  = format(res$allocation$redemption_date),
                     coupon   = sprintf("%.3f%%", res$allocation$coupon_rate),
                     invested = fmt(res$allocation$amount_invested),
                     units    = fmt(res$allocation$units),
                     weight   = sprintf("%6.2f%%", 100 * res$allocation$weight),
                     stringsAsFactors = FALSE),
          row.names = FALSE)
  } else {
    cat(" (empty portfolio)\n")
  }
  cat("=================================================================\n\n")

  invisible(res)
}

res = optimize_bond_ladder(10000000,4,Inflation_pa = 5.7,15)
print_ladder_optimization(res)
ladder_cashflow_schedule()
ladder_cashflow_schedule(res$bond_units,400000,5.7,15)

