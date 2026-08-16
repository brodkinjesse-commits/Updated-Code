# ==============================================================================
# REDINGTON IMMUNIZATION MODEL (PV, DURATION, & CONVEXITY CONSTRAINTS)
# ==============================================================================
# Solves for an optimal long-only bond portfolio meeting formal actuarial
# immunization conditions:
# 1. PV(Assets) >= PV(Liabilities)
# 2. Macaulay Duration(Assets) == Macaulay Duration(Liabilities)
# 3. Convexity(Assets) >= Convexity(Liabilities) + Convexity Buffer
#
# Liability structure: Annual withdrawals occurring at the END of each year
# (t = 1, 2, ..., ladder_years) -- Annuity Immediate.
# ==============================================================================

if (!require("lpSolve")) install.packages("lpSolve")
if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl")) install.packages("readxl")
library(lpSolve)
library(lubridate)
library(readxl)

MTM_FILE <- file.path("data", "Bond Data.xls")

# ------------------------------------------------------------------------------
# 1A. INPUT DATA: Bond market data from MTM extract
# ------------------------------------------------------------------------------
mtm_raw <- read_excel(MTM_FILE, sheet = "MTM", skip = 5)
mtm_raw <- as.data.frame(mtm_raw[!is.na(mtm_raw$`Bond Code`), ])

valuation_date <- as.Date(suppressMessages(read_excel(MTM_FILE, sheet = "MTM", range = "C4",
                                                      col_names = FALSE))[[1]][1])

bonds_market <- data.frame(
  bond_code             = mtm_raw$`Bond Code`,
  coupon_rate_nominal   = as.numeric(mtm_raw$Coupon),          # % p.a.
  redemption_date       = as.Date(mtm_raw$`Maturity`),
  redemption_amount_pct = 100,
  market_price          = as.numeric(mtm_raw$`All in price`),  # dirty price = actual cash outlay to buy the bond
  clean_price           = as.numeric(mtm_raw$`Clean Price`),
  mtm_duration          = as.numeric(mtm_raw$Duration),        # MTM-supplied duration - reference only, NOT used in the optimizer (see calculate_bond_metrics)
  mtm_convexity         = as.numeric(mtm_raw$Convexity),       # MTM-supplied convexity - reference only, NOT used in the optimizer
  stringsAsFactors = FALSE
)

bonds_fixed <- bonds_market
bonds_fixed$years_to_maturity <- as.numeric(bonds_fixed$redemption_date - valuation_date) / 365.25

# ------------------------------------------------------------------------------
# 1B. YIELD CURVE
# ------------------------------------------------------------------------------
# PLACEHOLDER: flat 6% curve until replaced with an actual fitted term
# structure. The BEASSA extract read + interpolation are kept below (commented)
# so swapping back in the real curve later is a one-line change.
#
# curve_raw <- read_excel(MTM_FILE, sheet = "BEASSA Yield Curve", skip = 6)
# names(curve_raw) <- c("t", "yield_pa")
# curve_raw <- curve_raw[!is.na(curve_raw$t), ]
# curve_raw <- curve_raw[order(curve_raw$t), ]
#
# curve_yield <- function(t_years) {
#   t_years <- pmax(t_years, min(curve_raw$t))
#   t_years <- pmin(t_years, max(curve_raw$t))
#   approx(curve_raw$t, curve_raw$yield_pa, xout = t_years)$y
# }

curve_yield <- function(t_years) {
  rep(6, length(t_years))   # flat 6% p.a. placeholder - replace with real term structure later
}

# ------------------------------------------------------------------------------
# 2. SINGLE-BOND CASH FLOW SCHEDULE (shared helper, per 1 unit / R100 par)
# ------------------------------------------------------------------------------
# Builds one bond's own semi-annual coupon + redemption cash flows, per unit
# held (i.e. per R100 par value). Shared by calculate_bond_metrics() (used
# for every bond in the universe, to price/duration/convexity them before
# the LP picks units) and bond_cashflow_schedule() (used for the specific
# bonds actually bought, scaled up by units_bought).
#
# The coupon cycle is anchored to the bond's OWN redemption_date (its real
# day-of-year), stepping backward in exact 6-month jumps until reaching
# valuation_date - NOT stepped forward from valuation_date, which would
# incorrectly force every bond's coupons onto valuation_date's day-of-year
# regardless of when that bond actually pays. Each date is computed
# directly from redemption_date (not chained from the previous date) using
# %m-%, so repeated month-end rollbacks can't drift the day of month over
# many periods. The final coupon coincides with redemption_date, and
# redemption is added onto that same cash flow. Each coupon payment is half
# the nominal annual rate (SA government bonds pay semi-annually).
single_bond_cashflows <- function(coupon_rate_nominal, redemption_amount_pct, redemption_date, valuation_date) {

  coupon_rate    <- coupon_rate_nominal / 100
  redemption_pct <- redemption_amount_pct / 100

  k <- 0
  repeat {
    nxt <- redemption_date %m-% months(6 * (k + 1))
    if (nxt <= valuation_date) break
    k <- k + 1
  }
  offsets <- seq(0, by = 6, length.out = k + 1)          # 0, 6, 12, ..., 6k months back
  dates   <- sort(redemption_date %m-% months(offsets))
  dates   <- dates[dates > valuation_date]

  coupon_amt <- 100 * coupon_rate / 2   # per 1 unit (R100 par), half nominal rate, twice a year
  cf         <- rep(coupon_amt, length(dates))
  cf[length(cf)] <- cf[length(cf)] + 100 * redemption_pct   # redemption on final cash flow

  type <- rep("Coupon", length(dates))
  type[length(type)] <- "Coupon + Redemption"

  t_years <- as.numeric(difftime(dates, valuation_date, units = "days")) / 365.25

  data.frame(date = dates, t_years = t_years, type = type, amount = cf, stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------------
# 3. ACTUARIAL METRICS: PV, Macaulay Duration, Convexity - calculated from
#    each bond's own real cash flows, not read from the MTM extract
# ------------------------------------------------------------------------------
# For every bond in the universe (per 1 unit / R100 par), builds its real
# semi-annual cash flow schedule via single_bond_cashflows(), discounts each
# cash flow at curve_yield() (its own actual timing, not a generic period
# count), and computes:
#   PV           = sum(CF_t / (1+y_t)^t)
#   Mac Duration = sum(t * PV_t) / PV                         (years)
#   Convexity    = sum(t*(t+1) * PV_t / (1+y_t)^2) / PV        (annualized)
# market_price stays as the actual quoted MTM price (what you'd pay to buy
# it) - PV is a separate, independently-computed theoretical value under
# curve_yield(); the two can legitimately differ if curve_yield() doesn't
# match the market's own implied yield, which is expected, not a bug.
calculate_bond_metrics <- function(bonds_fixed, valuation_date) {

  n_bonds <- nrow(bonds_fixed)
  metrics <- data.frame(
    bond_code    = bonds_fixed$bond_code,
    market_price = bonds_fixed$market_price,
    pv           = numeric(n_bonds),
    mac_duration = numeric(n_bonds),
    convexity    = numeric(n_bonds)
  )

  for (i in seq_len(n_bonds)) {
    cf <- single_bond_cashflows(
      coupon_rate_nominal   = bonds_fixed$coupon_rate_nominal[i],
      redemption_amount_pct = bonds_fixed$redemption_amount_pct[i],
      redemption_date       = bonds_fixed$redemption_date[i],
      valuation_date        = valuation_date
    )

    y    <- curve_yield(cf$t_years) / 100
    pv_t <- cf$amount / (1 + y)^cf$t_years

    pv_bond <- sum(pv_t)
    mac_dur <- sum(cf$t_years * pv_t) / pv_bond
    conv    <- sum(cf$t_years * (cf$t_years + 1) * pv_t / (1 + y)^2) / pv_bond

    metrics$pv[i]           <- pv_bond
    metrics$mac_duration[i] <- mac_dur
    metrics$convexity[i]    <- conv
  }

  metrics
}

# ------------------------------------------------------------------------------
# 4. LIABILITY CASH FLOWS: Annual withdrawals at END of each year (t = 1, 2, ..., N)
# ------------------------------------------------------------------------------
calculate_liability_metrics <- function(total_pot_value, withdrawal_rate_pct, ladder_years) {
  annual_withdrawal <- total_pot_value * (withdrawal_rate_pct / 100)

  # Paid at end of each year: t = 1, 2, ..., ladder_years
  t_vec  <- 1:ladder_years
  cf_vec <- rep(annual_withdrawal, ladder_years)
  y_vec  <- curve_yield(t_vec) / 100

  df_fac <- (1 + y_vec)^(-t_vec)
  pv_t   <- cf_vec * df_fac

  pv_liab      <- sum(pv_t)
  mac_dur_liab <- sum(t_vec * pv_t) / pv_liab
  conv_liab    <- sum(t_vec * (t_vec + 1) * pv_t / (1 + y_vec)^2) / pv_liab

  list(
    annual_withdrawal = annual_withdrawal,
    pv                = pv_liab,
    mac_duration      = mac_dur_liab,
    convexity         = conv_liab
  )
}

# ------------------------------------------------------------------------------
# 4. REDINGTON IMMUNIZATION OPTIMIZER
# ------------------------------------------------------------------------------
optimize_redington_immunization <- function(total_pot_value,
                                            withdrawal_rate_pct,
                                            ladder_years,
                                            convexity_buffer = 0.5,
                                            valuation_date = NULL,
                                            on_infeasible = c("stop", "flag"),
                                            bond_metrics_precomputed = NULL) {

  on_infeasible <- match.arg(on_infeasible)
  if (is.null(valuation_date)) valuation_date <- get("valuation_date", envir = .GlobalEnv)

  liab <- calculate_liability_metrics(total_pot_value, withdrawal_rate_pct, ladder_years)
  bond_metrics <- if (!is.null(bond_metrics_precomputed)) {
    bond_metrics_precomputed
  } else {
    calculate_bond_metrics(bonds_fixed, valuation_date)
  }

  # A bond that has already matured relative to this valuation_date has no
  # remaining cash flows: calculate_bond_metrics() correctly reports pv = 0
  # for it, which makes mac_duration = 0/0 = NaN (and convexity likewise).
  # That's an honest description of a matured bond (there's no meaningful
  # "duration" of zero future cash flows) - but it must never be a candidate
  # in the optimizer itself. Left in, that NaN was silently entering the
  # constraint matrix (pv=0 * NaN duration = NaN coefficient) without
  # necessarily crashing lp() - a silently wrong solve, not a loud failure -
  # and crashes outright when used directly as an objective coefficient
  # (as the relaxed fallback below does). Filtering here, once, keeps both
  # LP paths clean.
  bond_metrics <- bond_metrics[bond_metrics$pv > 0 & is.finite(bond_metrics$mac_duration), , drop = FALSE]
  if (nrow(bond_metrics) == 0) {
    msg <- "No bonds with remaining cash flows in the universe at this valuation date - every bond has matured."
    if (on_infeasible == "stop") stop(msg) else return(list(feasible = FALSE, message = msg, liability = liab))
  }
  n_bonds <- nrow(bond_metrics)

  obj_costs <- bond_metrics$market_price

  row_pv      <- bond_metrics$pv
  row_dur_eq  <- bond_metrics$pv * (bond_metrics$mac_duration - liab$mac_duration)
  target_conv <- liab$convexity + convexity_buffer
  row_conv    <- bond_metrics$pv * (bond_metrics$convexity - target_conv)

  const_matrix <- rbind(
    PV_Match       = row_pv,
    Duration_Match = row_dur_eq,
    Convexity_Diff = row_conv
  )
  const_dir <- c(">=", "==", ">=")
  const_rhs <- c(liab$pv, 0, 0)

  lp_res <- lp(direction = "min",
               objective.in = obj_costs,
               const.mat = const_matrix,
               const.dir = const_dir,
               const.rhs = const_rhs)

  duration_matched <- TRUE

  if (lp_res$status != 0) {
    # -------------------------------------------------------------------
    # FALLBACK: exact Redington duration-matching is only possible if the
    # bond universe has instruments spanning BOTH sides of the liability's
    # duration (a portfolio duration is a PV-weighted average of its
    # constituents, so it can never fall outside their range). When the
    # liability duration is shorter than every available bond's duration
    # (typical for a ladder nearing its own end), that's structurally
    # impossible - no amount of re-solving fixes it.
    #
    # Rather than give up on duration risk entirely, fall back to the
    # standard immunization-theory alternative: fully fund the liability
    # exactly (PV_A == PV_L, no more/no less - more capital doesn't reduce
    # a duration MISMATCH) and choose whichever bond mix gets portfolio
    # duration as close as possible to the target. Since total PV is now
    # fixed by an equality constraint, minimizing sum(units*pv*duration)
    # is equivalent to minimizing the PV-weighted average duration -
    # mechanically this concentrates into whichever bond has the duration
    # closest to (in this regime, just above) the liability's own.
    # Convexity is kept as a constraint, not relaxed - in the short-
    # duration regime it's typically easy to satisfy since even short
    # bonds carry more convexity than a short liability requires.
    # -------------------------------------------------------------------
    obj_min_dur <- bond_metrics$pv * bond_metrics$mac_duration

    const_matrix_relaxed <- rbind(
      PV_Match       = row_pv,
      Convexity_Diff = row_conv
    )
    const_dir_relaxed <- c("==", ">=")
    const_rhs_relaxed <- c(liab$pv, 0)

    lp_res <- lp(direction = "min",
                 objective.in = obj_min_dur,
                 const.mat = const_matrix_relaxed,
                 const.dir = const_dir_relaxed,
                 const.rhs = const_rhs_relaxed)

    duration_matched <- FALSE

    if (lp_res$status != 0) {
      msg <- paste("No feasible portfolio even under the relaxed (minimum duration",
                   "mismatch) fallback. This is a genuine infeasibility, not just a",
                   "duration-bracketing issue - check the bond universe and convexity buffer.",
                   "Liability duration:", round(liab$mac_duration, 4), "years.")
      if (on_infeasible == "stop") {
        stop(msg)
      } else {
        return(list(feasible = FALSE, message = msg, liability = liab))
      }
    }
  }

  bond_units <- lp_res$solution
  names(bond_units) <- bond_metrics$bond_code

  total_cost    <- sum(bond_units * bond_metrics$market_price)
  total_pv_a    <- sum(bond_units * bond_metrics$pv)
  portfolio_dur <- sum(bond_units * bond_metrics$pv * bond_metrics$mac_duration) / total_pv_a
  portfolio_cnv <- sum(bond_units * bond_metrics$pv * bond_metrics$convexity) / total_pv_a

  allocation_table <- data.frame(
    Bond_Code     = bond_metrics$bond_code,
    Price         = bond_metrics$market_price,
    Units_Bought  = round(bond_units, 4),
    Total_Cost    = round(bond_units * bond_metrics$market_price, 2),
    Weight_Pct    = round(((bond_units * bond_metrics$market_price) / total_cost) * 100, 2),
    PV_Weight_Pct = round(((bond_units * bond_metrics$pv) / total_pv_a) * 100, 2),
    Mac_Duration  = round(bond_metrics$mac_duration, 2),
    Convexity     = round(bond_metrics$convexity, 2)
  )

  actuarial_summary <- data.frame(
    Metric = c("Present Value (PV)", "Macaulay Duration (Years)", "Annualized Convexity"),
    Liabilities = round(c(liab$pv, liab$mac_duration, liab$convexity), 4),
    Immunized_Portfolio = round(c(total_pv_a, portfolio_dur, portfolio_cnv), 4),
    Status = c(
      ifelse(total_pv_a >= liab$pv - 1e-6, "PASSED (PV_A >= PV_L)", "FAILED"),
      if (duration_matched) {
        ifelse(abs(portfolio_dur - liab$mac_duration) < 0.001, "MATCHED (D_A == D_L)", "FAILED")
      } else {
        sprintf("MINIMIZED, NOT MATCHED (gap = %.4f yrs - no bond short enough)",
                portfolio_dur - liab$mac_duration)
      },
      ifelse(portfolio_cnv >= liab$convexity + convexity_buffer - 1e-6,
             "PASSED (C_A >= C_L + buffer)", "FAILED")
    )
  )

  return(list(
    feasible = TRUE,
    duration_matched = duration_matched,
    total_cost = total_cost,
    liability = liab,
    bond_metrics = bond_metrics,
    bond_units = bond_units,
    actuarial_summary = actuarial_summary,
    portfolio_allocation = allocation_table[allocation_table$Units_Bought > 0, ]
  ))
}

# ------------------------------------------------------------------------------
# 6. BOND LADDER CASH FLOW SCHEDULE (for verification/plotting)
# ------------------------------------------------------------------------------
# Builds the actual coupon + redemption cash flows the immunized bond
# portfolio pays out over time, scaled up from single_bond_cashflows() (per
# 1 unit / R100 par) by each bond's units_bought.
#
# t_years (exact fractional years from valuation_date) and month_index (the
# exact simulation month, 1..horizon, the cash flow lands in) are kept
# unrounded for use in discounting and cash-flow placement; `year` is a
# label rounded to the nearest HALF-year (0.5, 1.0, 1.5, ...) kept only for
# display/grouping in tables and plots, so the two semi-annual coupons in
# the same year show up as separate entries rather than being merged into
# one - it should never be used in a calculation.
bond_cashflow_schedule <- function(portfolio_allocation, bonds_fixed, valuation_date) {

  do.call(rbind, lapply(seq_len(nrow(portfolio_allocation)), function(i) {

    bcode <- portfolio_allocation$Bond_Code[i]
    units <- portfolio_allocation$Units_Bought[i]
    b_row <- bonds_fixed[bonds_fixed$bond_code == bcode, ]

    cf <- single_bond_cashflows(
      coupon_rate_nominal   = b_row$coupon_rate_nominal,
      redemption_amount_pct = b_row$redemption_amount_pct,
      redemption_date       = b_row$redemption_date,
      valuation_date        = valuation_date
    )

    data.frame(
      bond_code   = bcode,
      date        = cf$date,
      t_years     = cf$t_years,
      month_index = round(cf$t_years * 12),           # exact simulation month this cash flow falls in
      year        = round(cf$t_years * 2) / 2,         # display/grouping label ONLY - not for calculations
      type        = cf$type,
      amount      = cf$amount * units,                 # scale from per-unit (R100 par) up to units actually bought
      stringsAsFactors = FALSE
    )
  }))
}

# ------------------------------------------------------------------------------
# 7. BOND LADDER SALE VALUE AT LADDER END
# ------------------------------------------------------------------------------
# Some bonds in the immunized portfolio mature after the ladder ends (Redington
# immunization matches PV/duration/convexity, not exact cash-flow timing, so
# this is expected). Rather than hold them to maturity, they're sold once the
# ladder ends - this values that sale as the PV, at the ladder-end date, of
# just those post-ladder cash flows, discounted using curve_yield().
#
# Uses the bond's actual calendar date vs the actual ladder-end date
# throughout - no rounding to whole years - so a cash flow due, say, 3
# months after the ladder ends is discounted over 0.25 years, not bucketed
# into a whole extra year.
#
# NOTE: this is a static/deterministic value (identical across every
# simulation) since curve_yield() is currently a flat placeholder. Once a
# real stochastic term structure is built, curve_yield() (and this function)
# can be made simulation-specific so the sale value varies by simulation too.
bond_ladder_sale_value <- function(bond_cf, valuation_date, ladder_years) {
  ladder_end_date <- valuation_date + lubridate::years(ladder_years)
  future_cf <- bond_cf[bond_cf$date > ladder_end_date, ]
  if (nrow(future_cf) == 0) return(0)
  t_remaining <- as.numeric(difftime(future_cf$date, ladder_end_date, units = "days")) / 365.25
  y <- curve_yield(t_remaining) / 100
  sum(future_cf$amount / (1 + y)^t_remaining)
}

# ------------------------------------------------------------------------------
# 8. LADDER CASH BUFFER SIZING
# ------------------------------------------------------------------------------
# Bond coupons/redemption arrive on their own real calendar dates (lumpy),
# but the retiree draws a level income every month. This sizes the initial
# cash balance - invested at cash_annual_rate, compounded monthly - needed
# so a cash account that receives each ladder cash flow in the exact
# simulation month it actually falls in (bond_cf$month_index, not a rounded
# year bucket) and pays out annual_withdrawal/12 every month never goes
# negative across the whole ladder period.
#
# Because interest is the only balance-dependent term in this account (the
# deposit and withdrawal schedule doesn't depend on the balance), the effect
# of the starting buffer C0 on the balance at month m is a pure compounding
# factor: balance_track[m] = balance_track_with_zero_start[m] + C0*(1+r)^(m-1).
# That makes the minimum required C0 solvable in closed form rather than by
# search - just simulate once from zero and back out the worst shortfall.
size_ladder_cash_buffer <- function(bond_cf, ladder_years, annual_withdrawal, cash_annual_rate) {

  cash_monthly_rate <- (1 + cash_annual_rate)^(1/12) - 1
  months <- ladder_years * 12
  monthly_withdrawal <- annual_withdrawal / 12

  # Each cash flow lands in its own exact simulation month (bond_cf$month_index).
  ladder_cf <- bond_cf[bond_cf$month_index <= months, ]
  deposits_by_month <- rep(0, months)
  if (nrow(ladder_cf) > 0) {
    for (k in seq_len(nrow(ladder_cf))) {
      m <- min(max(ladder_cf$month_index[k], 1), months)  # clamp as a safety guard
      deposits_by_month[m] <- deposits_by_month[m] + ladder_cf$amount[k]
    }
  }

  # Simulate with a zero starting balance to see how negative it would get
  balance <- 0
  balance_track <- numeric(months)
  for (m in 1:months) {
    balance <- balance + deposits_by_month[m] - monthly_withdrawal
    balance_track[m] <- balance          # pre-interest: the tightest point in the month
    balance <- balance * (1 + cash_monthly_rate)
  }

  growth_factors <- (1 + cash_monthly_rate)^(0:(months - 1))
  required_C0 <- max(0, max(-balance_track / growth_factors))

  list(
    C0                       = required_C0,
    cash_monthly_rate        = cash_monthly_rate,
    deposits_by_month        = deposits_by_month,
    monthly_withdrawal       = monthly_withdrawal,
    balance_track_zero_start = balance_track
  )
}
