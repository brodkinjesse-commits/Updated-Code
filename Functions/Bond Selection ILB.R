# ==============================================================================
# BOND SELECTION (ILB): a cash-flow-matched ladder built entirely in REAL terms
# ==============================================================================
# The real-terms counterpart to `Functions/Bond Selection.R`. That file buys
# NOMINAL government bonds against an income stream escalated at an assumed
# inflation rate. This one buys INFLATION-LINKED bonds (IGOV) against a LEVEL
# REAL income, which is the same promise made honestly: an ILB's coupon and
# redemption are contractually indexed to CPI, so a ladder that matches a level
# real liability delivers a CPI-indexed nominal income whatever inflation
# actually does. The escalation assumption disappears from the liability
# entirely - see "Where inflation_rate is and is not used" below.
#
# EVERYTHING INSIDE THIS FILE IS IN REAL RANDS AS AT `start_date`. Nothing else
# in the project is. The bridge between the two worlds is that a real rand at
# start_date IS a nominal rand at start_date - they are the same money on the
# same day - so the rand amounts this file returns (total_cost, cash_at_start,
# total_outlay, bond_amounts) are directly consumable by the nominal machinery
# without conversion. The month-by-month SCHEDULE is not: those flows are real
# and would need reflating by a CPI path before they mean anything nominal.
# Wiring any of this into Dynamic_Ladder.R / Thesis.R is a separate task; this
# file deliberately sources nothing and is sourced by nothing.
#
# THE ALGORITHM - backward dedication, one bond per maturity gap
#
#   Cut the ladder into BLOCKS at the redemption months of the eligible ILBs.
#   Block g opens in the month its bond redeems and closes the month before the
#   next bond redeems; the last block runs to the end of the ladder. A block is
#   therefore exactly as long as the maturity gap that follows its bond. Walk
#   BACKWARDS from the last block to the first. For each block:
#
#     - work out what it still needs: its real withdrawals, less whatever the
#       bonds already bought (all of which mature LATER) pay into it as coupons;
#     - if that is already >= 0, the block is covered by carry alone. Buy
#       nothing. It is solved;
#     - otherwise buy exactly enough of the bond redeeming in its first month to
#       close the gap to zero.
#
#   Walking backwards is what makes each block's answer final: the bond bought
#   for block g redeems in the FIRST month of block g, so it pays nothing after
#   the block and can only ever reduce what the EARLIER blocks need. No block is
#   ever revisited.
#
#   Long only. If carry overshoots a block, the surplus stays in the bank account
#   and flows back to the earlier blocks; it is never sold short.
#
# WHY BLOCKS ARE CUT AT REDEMPTION MONTHS
#
#   Because it is what decides how much of the ladder sits in cash, and the ILB
#   maturity grid is full of holes - nothing redeems before Mar 2028, nothing
#   between Dec 2033 and Jan 2038.
#
#   Two things have to be true of a block: its bond must be able to reach every
#   month in it, and no month should wait longer than it must. Cutting on
#   redemption months gives both. Money for a stretch with no ILB redeeming in it
#   is carried back onto the bond BEFORE that stretch - bought bigger, redeeming
#   early, then held in the bank until spent - instead of being parked in cash
#   from day zero. And because a block opens ON its redemption, no month inside
#   it is ever waiting for money that has not arrived.
#
#   The alternative cuts are both worse, and measurably so. Calendar-year buckets
#   with no carry-back (the first version of this file) leave a gap year with no
#   bond at all, so its whole year of income must be pre-funded in cash: on a 10y
#   ladder that put 51.8% of the t0 outlay in a bank account earning nothing in
#   real terms. Carrying back onto annual buckets fixes most of that but leaves
#   the intra-bucket lead time - a bucket opening in month 85 whose bond does not
#   redeem until month 89 needs four months of cash to bridge it - which then
#   becomes the binding constraint and strands the surplus it creates. Cutting on
#   redemption months removes the lead time entirely. On the base case the three
#   cuts cost R4.56m, R4.39m and R4.21m of t0 outlay respectively.
#
#   What no cut can fix is the run of months BEFORE the first redemption. Nothing
#   in the universe pays there, so it is funded from opening cash at any price -
#   see cash_only_months and the on_infeasible note in section 7.
#
#   The nominal file's optimize_bond_ladder() matches month by month via an LP.
#   This one does not need to: with ten bonds on a lumpy grid the backward pass
#   IS the optimum for a level real liability, it is deterministic, and it says
#   plainly which bond is funding which months.
#
# WHERE inflation_rate IS AND IS NOT USED
#
#   NOT used to escalate the liability. `annual_real_withdrawal` is level in
#   real terms by construction - that is the whole point of working in real
#   space, and it is the one deliberate divergence from Bond Selection.R, whose
#   `Inflation_pa = 5.7` escalates the nominal instalment month by month.
#
#   USED only to derive the real rate credited on idle cash, via Fisher:
#       real_interest_rate = (1 + nominal_interest_rate)/(1 + inflation_rate) - 1
#   `nominal_interest_rate` is the project's existing `cash_annual_rate` and
#   `inflation_rate` its existing flat inflation assumption. Deriving rather
#   than taking a free real rate is what keeps this file's cash arithmetic
#   consistent with the nominal cash account in Dynamic_Ladder.R: the same two
#   inputs drive both, so they cannot drift.
#
# UNITS - read this before using bond_units downstream
#
#   ONE UNIT = R100 of START-DATE-INDEXED par, i.e. par already grossed up by
#   the bond's index ratio. In that unit the real cash flows are the plain
#   quoted numbers - R(coupon/2) twice a year and R100 at redemption - and the
#   real price is market_price / index_ratio. This is the natural unit for real
#   cash-flow matching and it is what `bond_units` and the schedule are in.
#
#   It is NOT the unit a broker trades in. Base (original) par is
#       base_par = units * 100 / index_ratio
#   which is reported as `bond_units_base_par` and in the allocation table. The
#   two agree on money: base_par/100 * market_price == units * real_price.
# ==============================================================================

if (!require("lubridate")) install.packages("lubridate")
if (!require("readxl"))    install.packages("readxl")
library(lubridate)
library(readxl)

ILB_FILE  <- file.path("Data", "ILB Bond data.xlsx")   # exact on-disk casing - matters on a case-sensitive filesystem
ILB_SHEET <- "Bond Data"

# Time zero for the ILB ladder: 1 August 2026. The date the ladder is bought,
# the date the first real withdrawal is paid, and the origin every month index
# and year period is measured from.
#
# The nominal file reads its t0 out of the MTM extract's own valuation cell (C4)
# and stops if it is not MODEL_START_DATE. This workbook has no valuation-date
# cell, so there is nothing to read and the guard has to work the other way
# round: the date is asserted here, and optimize_ilb_ladder() stops if it is
# handed a different `start_date`. Prices and time zero must be the same day.
#
# NOTE FOR THE WIRING TASK: this is 2026-08-01, one day after the nominal file's
# MODEL_START_DATE of 2026-07-31. Harmless in isolation, but the two ladders
# cannot share a t0 until one of them moves. Warned about at source time below.
ILB_MODEL_START_DATE <- as.Date("2026-08-01")

# The CPI reference level the workbook's index ratios are struck off. Every
# ratio in the sheet is (ref / base_cpi), so ratio * base_cpi must recover this
# for all ten bonds - checked below.
ILB_REFERENCE_CPI <- 107.5

# ------------------------------------------------------------------------------
# 1. ILB UNIVERSE
# ------------------------------------------------------------------------------
# Columns are matched by pattern, not by exact name: the sheet carries headers
# like "Index Ratio (107.5/Base)" and "implied real price (market_price / ratio)"
# whose text is documentation as much as identifier, and one of them contains a
# non-ASCII division sign. Anchored patterns keep "^market_price" from also
# matching the "...(market_price / ratio)" column.
ilb_pick_col <- function(df, pattern, what) {
  hit <- grep(pattern, names(df), ignore.case = TRUE, perl = TRUE)
  if (length(hit) != 1L) {
    stop(sprintf(paste0("ILB workbook: expected exactly one '%s' column matching /%s/ in sheet ",
                        "'%s', found %d. Columns present: %s"),
                 what, pattern, ILB_SHEET, length(hit), paste(names(df), collapse = " | ")))
  }
  df[[hit]]
}

ilb_raw <- as.data.frame(suppressMessages(read_excel(ILB_FILE, sheet = ILB_SHEET)))
ilb_raw <- ilb_raw[!is.na(ilb_pick_col(ilb_raw, "^bond[_ ]?code", "bond code")), , drop = FALSE]

ilb_fixed <- data.frame(
  bond_code             = as.character(ilb_pick_col(ilb_raw, "^bond[_ ]?code", "bond code")),
  coupon_rate_real      = as.numeric(ilb_pick_col(ilb_raw, "^coupon", "coupon")),        # % p.a. REAL, semi-annual
  redemption_date       = as.Date(ilb_pick_col(ilb_raw, "^redemption[_ ]?date", "redemption date")),
  redemption_amount_pct = as.numeric(ilb_pick_col(ilb_raw, "^redemption[_ ]?amount", "redemption amount %")),
  market_price          = as.numeric(ilb_pick_col(ilb_raw, "^market[_ ]?price", "market price")),  # NOMINAL all-in, per R100 base par
  base_cpi              = as.numeric(ilb_pick_col(ilb_raw, "^base[_ ]?cpi", "base CPI")),
  index_ratio           = as.numeric(ilb_pick_col(ilb_raw, "^index[_ ]?ratio", "index ratio")),
  stringsAsFactors      = FALSE
)

# The one line that moves the whole file into real space. market_price is the
# NOMINAL all-in price - real price times the bond's own index ratio - and those
# ratios run from 1.14 (a recent issue) to 3.19 (R202, issued 2003). The error
# does not wash out across bonds: taking the quoted prices at face value in real
# terms makes R202 and R210 look roughly three times cheaper per rand of real
# redemption than they are, and the backward pass would pile into them.
ilb_fixed$real_price        <- ilb_fixed$market_price / ilb_fixed$index_ratio
ilb_fixed$years_to_maturity <- as.numeric(ilb_fixed$redemption_date - ILB_MODEL_START_DATE) / 365.25

# The t0 index ratio, frozen under its own name. `index_ratio` is an
# AS-AT-A-DATE quantity: Functions/ILB_Repricing.R's reprice_universe()
# overwrites it with CPI(t)/base_cpi when it reprices the universe at a future
# review. `units`, by contrast, are defined ONCE off the t0 ratio (one unit =
# R100 of START-DATE-indexed par - see the UNITS note in this file's header)
# and are never re-based, so anything converting between units and ORIGINAL par
# must read this column, not the live one.
ilb_fixed$index_ratio_t0    <- ilb_fixed$index_ratio

# --- load-time guards -------------------------------------------------------
local({
  bad <- function(cond, msg) if (anyNA(cond) || any(cond)) stop("ILB workbook: ", msg)

  bad(!is.finite(ilb_fixed$coupon_rate_real) | ilb_fixed$coupon_rate_real < 0,
      "coupon rates must be finite and non-negative")
  bad(is.na(ilb_fixed$redemption_date), "every bond needs a redemption date")
  bad(!is.finite(ilb_fixed$market_price) | ilb_fixed$market_price <= 0,
      "market prices must be finite and positive")
  bad(!is.finite(ilb_fixed$base_cpi) | ilb_fixed$base_cpi <= 0,
      "base CPI must be finite and positive")
  bad(!is.finite(ilb_fixed$index_ratio) | ilb_fixed$index_ratio <= 0,
      "index ratios must be finite and positive")
  bad(!is.finite(ilb_fixed$redemption_amount_pct) | ilb_fixed$redemption_amount_pct <= 0,
      "redemption amounts must be finite and positive")
  if (anyDuplicated(ilb_fixed$bond_code)) stop("ILB workbook: duplicate bond codes")

  # ratio * base must recover the reference CPI for every bond. This is the
  # guard that fires if someone refreshes base CPIs without recomputing the
  # ratio column, or restrikes the ratios off a newer CPI print without moving
  # ILB_REFERENCE_CPI. The ratios are stored to 3dp, so allow 0.5%.
  implied <- ilb_fixed$index_ratio * ilb_fixed$base_cpi
  off     <- abs(implied / ILB_REFERENCE_CPI - 1) > 0.005
  if (any(off)) {
    stop(sprintf(paste0("index_ratio * base_cpi does not recover the reference CPI of %.2f for: %s ",
                        "(implied %s). Either the ratios are stale or ILB_REFERENCE_CPI is."),
                 ILB_REFERENCE_CPI, paste(ilb_fixed$bond_code[off], collapse = ", "),
                 paste(sprintf("%.2f", implied[off]), collapse = ", ")))
  }

  # cross-check our real prices against the sheet's own "implied real price"
  # column where it exists. It is rounded to 1dp, hence the loose tolerance -
  # this is here to catch a column swap, not to audit arithmetic.
  shown <- tryCatch(as.numeric(ilb_pick_col(ilb_raw, "implied[_ ]?real[_ ]?price", "implied real price")),
                    error = function(e) NULL)
  if (!is.null(shown)) {
    drift <- abs(ilb_fixed$real_price / shown - 1) > 0.005
    if (any(drift, na.rm = TRUE)) {
      warning("ILB workbook: computed real prices differ from the sheet's 'implied real price' column for ",
              paste(ilb_fixed$bond_code[which(drift)], collapse = ", "),
              " - check the index ratio column has not moved.")
    }
  }

  if (all(ilb_fixed$redemption_date <= ILB_MODEL_START_DATE)) {
    stop(sprintf("every ILB in the universe has already redeemed by the model start date (%s)",
                 format(ILB_MODEL_START_DATE)))
  }

  # Index ratios are stored to 3dp, which costs up to ~0.05% on a real price
  # (~R5k on a R10m ladder). Immaterial, but if it ever needs to go away, store
  # base_cpi and the reference CPI and compute the ratio at full precision here.

  if (exists("MODEL_START_DATE", envir = .GlobalEnv)) {
    nominal_t0 <- get("MODEL_START_DATE", envir = .GlobalEnv)
    if (!identical(as.Date(nominal_t0), ILB_MODEL_START_DATE)) {
      warning(sprintf(paste0("ILB_MODEL_START_DATE (%s) differs from the nominal MODEL_START_DATE (%s). ",
                             "Fine while this file is standalone; the two ladders must share a t0 before ",
                             "they can be run side by side."),
                      format(ILB_MODEL_START_DATE), format(as.Date(nominal_t0))))
    }
  }
})

# ------------------------------------------------------------------------------
# 2. FISHER: the real rate earned on idle cash
# ------------------------------------------------------------------------------
# Rates are DECIMALS (0.05 = 5%), matching cash_annual_rate and inflation_rate
# in Thesis.R - NOT percentages, which is the convention Bond Selection.R's
# Inflation_pa uses. Getting that wrong is silent and expensive, so anything
# above 1 is rejected as an obvious percent-for-decimal slip.
fisher_real_rate <- function(nominal_interest_rate, inflation_rate) {

  if (!is.finite(nominal_interest_rate) || !is.finite(inflation_rate)) {
    stop("nominal_interest_rate and inflation_rate must both be finite")
  }
  if (nominal_interest_rate <= -1 || inflation_rate <= -1) {
    stop("nominal_interest_rate and inflation_rate must be greater than -1 (i.e. above -100%)")
  }
  if (nominal_interest_rate > 1 || inflation_rate > 1) {
    stop(sprintf(paste0("nominal_interest_rate (%.4g) and inflation_rate (%.4g) must be DECIMALS, not ",
                        "percentages - 0.05 means 5%%. Values above 1 are rejected as a likely slip."),
                 nominal_interest_rate, inflation_rate))
  }

  (1 + nominal_interest_rate) / (1 + inflation_rate) - 1
}

# ------------------------------------------------------------------------------
# 3. CALENDAR BUCKETING: cash flow date -> simulation month, and month -> year
# ------------------------------------------------------------------------------
# Deliberately a self-contained copy of Bond Selection.R's month_index_from_date()
# rather than a shared helper, for two reasons: this file must source cleanly on
# its own, and sourcing it must not shadow the nominal selector's identically
# named function if both are loaded during the wiring task. Hence the _ilb suffix
# on every name here that has a nominal twin.
#
# Whole months are counted by calendar arithmetic, not by rounding t_years * 12,
# which credits a flow up to ~2 weeks early. The year/month difference is
# corrected by at most one step because %m+% clamps to month end.
months_elapsed_ilb <- function(dates, start_date) {

  dates      <- as.Date(dates)
  start_date <- as.Date(start_date)

  k <- 12L * (year(dates) - year(start_date)) + (month(dates) - month(start_date))

  too_far  <- start_date %m+% months(k) > dates
  k[too_far] <- k[too_far] - 1L
  too_near <- start_date %m+% months(k + 1L) <= dates
  k[too_near] <- k[too_near] + 1L

  as.integer(k)
}

# Month m spans [t0 %m+% months(m-1), t0 %m+% months(m)):
#   month 1 = 1 Aug 2026 -> 31 Aug 2026,  month 2 = 1 Sep 2026 -> 30 Sep 2026, ...
# Floored at month 2: the ladder is bought at t0 and the first real withdrawal is
# paid at t0, but the earliest a bond can pay is a full month later, so anything
# landing inside month 1 is credited no earlier than it can actually arrive.
month_index_from_date_ilb <- function(dates, start_date, min_month = 2L) {
  pmax(as.integer(min_month), months_elapsed_ilb(dates, start_date) + 1L)
}

# Period p spans [t0 %m+% years(p-1), t0 %m+% years(p)):
#   period 1 = Aug 2026 -> Jul 2027,  period 2 = Aug 2027 -> Jul 2028, ...
# Derived from the SAME month count as month_index_from_date_ilb(), so the yearly
# buckets the bond-selection pass works in and the monthly buckets the bank
# balance works in can never disagree about which year a flow falls in. (A flow
# inside month 1 is floored to month 2, which is still period 1 - so the floor
# cannot move a flow between periods either.)
year_period_from_date <- function(dates, start_date) {
  months_elapsed_ilb(dates, start_date) %/% 12L + 1L
}

# ------------------------------------------------------------------------------
# 4. ONE ILB'S REAL CASH FLOWS (per 1 unit = R100 of start-date-indexed par)
# ------------------------------------------------------------------------------
# In real terms an ILB is just a plain vanilla bond: R(coupon/2) twice a year and
# R100 at redemption, per R100 of indexed par. All the CPI machinery lives in the
# index ratio, which has already been divided out of the price.
#
# The coupon cycle is anchored to the bond's OWN redemption date, stepping
# backward in exact 6-month jumps - not stepped forward from start_date, which
# would force every bond's coupons onto start_date's day of month. Each date is
# computed directly from redemption_date via %m-% rather than chained off the
# previous one, so repeated month-end rollbacks cannot drift the day of month
# over sixty periods.
#
# SIMPLIFICATION: SA ILBs index off a 4-month-lagged, daily-interpolated CPI, so
# a cash flow is fixed to the price level of four months earlier and is not
# EXACTLY constant in real terms. There is nothing in the workbook to model that
# lag with, and the resulting basis is second order next to the flat-curve and
# level-liability assumptions already in the project. Treated as exactly real.
single_ilb_real_cashflows <- function(coupon_rate_real, redemption_amount_pct,
                                      redemption_date, start_date) {

  empty <- data.frame(date = as.Date(character(0)), t_years = numeric(0),
                      type = character(0), amount = numeric(0), stringsAsFactors = FALSE)
  if (redemption_date <= start_date) return(empty)

  k <- 0
  repeat {
    nxt <- redemption_date %m-% months(6 * (k + 1))
    if (nxt <= start_date) break
    k <- k + 1
  }
  dates <- sort(redemption_date %m-% months(seq(0, by = 6, length.out = k + 1)))
  dates <- dates[dates > start_date]
  if (!length(dates)) return(empty)

  coupon_amt     <- 100 * (coupon_rate_real / 100) / 2
  cf             <- rep(coupon_amt, length(dates))
  cf[length(cf)] <- cf[length(cf)] + 100 * (redemption_amount_pct / 100)

  type               <- rep("Coupon", length(dates))
  type[length(type)] <- "Coupon + Redemption"

  data.frame(date    = dates,
             t_years = as.numeric(difftime(dates, start_date, units = "days")) / 365.25,
             type    = type,
             amount  = cf,
             stringsAsFactors = FALSE)
}

# ------------------------------------------------------------------------------
# 5. LIABILITY: a LEVEL real income, paid monthly in advance
# ------------------------------------------------------------------------------
# Annuity-due: the first instalment is drawn at t = 0 and the last at the start
# of the final month. No escalation - level in real terms is the point. Accepts
# either the rand amount directly or the (pot, rate) pair the rest of the project
# thinks in; exactly one of the two forms must be given.
ilb_liability <- function(ladder_years,
                          annual_real_withdrawal = NULL,
                          total_pot_value        = NULL,
                          withdrawal_rate_pct    = NULL) {

  by_amount <- !is.null(annual_real_withdrawal)
  by_rate   <- !is.null(total_pot_value) || !is.null(withdrawal_rate_pct)

  if (by_amount && by_rate) {
    stop("give EITHER annual_real_withdrawal OR (total_pot_value, withdrawal_rate_pct), not both")
  }
  if (!by_amount && !by_rate) {
    stop("give either annual_real_withdrawal, or both total_pot_value and withdrawal_rate_pct")
  }
  if (by_rate) {
    if (is.null(total_pot_value) || is.null(withdrawal_rate_pct)) {
      stop("the rate form needs BOTH total_pot_value and withdrawal_rate_pct")
    }
    if (!is.finite(total_pot_value) || total_pot_value <= 0) stop("total_pot_value must be positive")
    if (!is.finite(withdrawal_rate_pct) || withdrawal_rate_pct <= 0) {
      stop("withdrawal_rate_pct must be positive, in PERCENT (5 means 5%)")
    }
    annual_real_withdrawal <- total_pot_value * (withdrawal_rate_pct / 100)
  }

  if (!is.finite(annual_real_withdrawal) || annual_real_withdrawal <= 0) {
    stop("the real liability is zero, negative or non-finite - nothing to fund")
  }
  if (!is.finite(ladder_years) || ladder_years <= 0) stop("ladder_years must be positive")

  n_months  <- max(1L, as.integer(round(ladder_years * 12)))
  n_periods <- as.integer(ceiling(n_months / 12))

  list(annual_real_withdrawal  = annual_real_withdrawal,
       monthly_real_withdrawal = annual_real_withdrawal / 12,
       total_pot_value         = total_pot_value,
       withdrawal_rate_pct     = withdrawal_rate_pct,
       ladder_years            = ladder_years,
       n_months                = n_months,
       n_periods               = n_periods,
       total_withdrawn         = (annual_real_withdrawal / 12) * n_months)
}

# ------------------------------------------------------------------------------
# 6. THE LADDER'S REAL CASH SCHEDULE AND THE BANK ACCOUNT
# ------------------------------------------------------------------------------
# Month-by-month real cash flow of a given holding, rolled through a bank account
# earning the real rate.
#
# Within-month order of events, which is what makes the numbers reproducible:
#   1. the real withdrawal is paid at the START of the month (annuity-due);
#   2. that month's bond inflows are credited;
#   3. the closing balance earns one month's real interest, becoming the next
#      month's opening balance.
# Non-negativity is tested on the CLOSING balance, which is the tighter point
# because the withdrawal has already gone out.
#
# With C in cash at the start, the closing balance in month m is
#     bal_m(C) = C*(1+r)^(m-1) + D_m,      D_m = sum_{k<=m} net_k * (1+r)^(m-k)
# where net_k is that month's inflow less its withdrawal. So the smallest C that
# keeps every bal_m >= 0 is available in closed form:
#     C* = max(0, max_m [ -D_m / (1+r)^(m-1) ])
# and at the binding month bal_m is exactly 0 - which is what "minimum balance
# over the whole period = 0 at its lowest point" means. No search, no iteration.
#
# Because no bond can pay in month 1 while the first withdrawal is paid in it,
# D_1 = -W and C* >= W always: the ladder can never start with less than one
# monthly instalment in the bank, exactly as in the nominal file.
#
# `starting_cash` is left NULL to solve for C*; pass a number to test a different
# assumption against it.
ilb_ladder_real_schedule <- function(bond_units,
                                     annual_real_withdrawal,
                                     ladder_years,
                                     real_interest_rate,
                                     bonds         = ilb_fixed,
                                     start_date    = ILB_MODEL_START_DATE,
                                     starting_cash = NULL) {

  # Holdings are matched to bonds BY NAME. An unnamed vector or a typo'd code
  # would otherwise silently match nothing and surface much later as an
  # unexplained shortfall.
  if (length(bond_units) > 0) {
    if (is.null(names(bond_units))) {
      stop("bond_units must be a NAMED vector of bond codes, e.g. c(I2029 = 1234.5). ",
           "Holdings are matched to bonds by name, not by position.")
    }
    unknown <- setdiff(names(bond_units)[!is.na(bond_units) & bond_units > 0], bonds$bond_code)
    if (length(unknown)) {
      stop("unknown bond code(s): ", paste(unknown, collapse = ", "),
           ". Valid codes: ", paste(bonds$bond_code, collapse = ", "))
    }
    if (any(bond_units < 0, na.rm = TRUE)) {
      stop("negative holdings in bond_units - the ILB ladder is long only")
    }
  }
  if (!is.finite(real_interest_rate) || real_interest_rate <= -1) {
    stop("real_interest_rate must be finite and greater than -1")
  }

  lia      <- ilb_liability(ladder_years, annual_real_withdrawal = annual_real_withdrawal)
  n_months <- lia$n_months
  months_v <- seq_len(n_months)
  W        <- lia$monthly_real_withdrawal

  held <- bond_units[!is.na(bond_units) & bond_units > 0]

  bond_flows <- if (length(held) == 0) NULL else {
    do.call(rbind, lapply(names(held), function(bcode) {
      b  <- bonds[bonds$bond_code == bcode, ]
      cf <- single_ilb_real_cashflows(b$coupon_rate_real, b$redemption_amount_pct,
                                      b$redemption_date, start_date)
      if (!nrow(cf)) return(NULL)
      data.frame(bond_code   = bcode,
                 date        = cf$date,
                 month_index = month_index_from_date_ilb(cf$date, start_date),
                 period      = year_period_from_date(cf$date, start_date),
                 type        = cf$type,
                 amount      = cf$amount * held[[bcode]],   # per R100 indexed par -> units held
                 stringsAsFactors = FALSE)
    }))
  }
  if (is.null(bond_flows)) {
    bond_flows <- data.frame(bond_code = character(0), date = as.Date(character(0)),
                             month_index = integer(0), period = integer(0),
                             type = character(0), amount = numeric(0), stringsAsFactors = FALSE)
  }
  if (nrow(bond_flows)) bond_flows <- bond_flows[order(bond_flows$date, bond_flows$bond_code), ]

  in_ladder     <- if (nrow(bond_flows)) bond_flows$month_index <= n_months else logical(0)
  is_redemption <- if (nrow(bond_flows)) grepl("Redemption", bond_flows$type, fixed = TRUE) else logical(0)

  bucket <- function(rows) {
    if (!any(rows)) return(rep(0, n_months))
    as.numeric(tapply(bond_flows$amount[rows],
                      factor(bond_flows$month_index[rows], levels = months_v),
                      sum, default = 0))
  }

  schedule <- data.frame(
    month         = months_v,
    month_start   = start_date %m+% months(months_v - 1L),
    period        = (months_v - 1L) %/% 12L + 1L,
    bond_inflow   = bucket(in_ladder),
    coupon_in     = bucket(in_ladder & !is_redemption),
    redemption_in = bucket(in_ladder & is_redemption),
    withdrawal    = rep(W, n_months),
    stringsAsFactors = FALSE
  )
  schedule$net_cashflow <- schedule$bond_inflow - schedule$withdrawal
  schedule$cum_net      <- cumsum(schedule$net_cashflow)   # undiscounted, no interest

  # roll the bank account
  r      <- (1 + real_interest_rate)^(1 / 12) - 1
  growth <- (1 + r)^(months_v - 1L)

  D   <- numeric(n_months)
  acc <- 0
  for (m in months_v) {
    acc  <- acc * (1 + r) + schedule$net_cashflow[m]
    D[m] <- acc
  }

  solved_cash <- max(0, max(-D / growth))
  if (is.null(starting_cash)) starting_cash <- solved_cash
  if (!is.finite(starting_cash) || starting_cash < 0) stop("starting_cash must be finite and non-negative")

  schedule$balance      <- starting_cash * growth + D          # closing balance
  schedule$balance_open <- c(starting_cash, schedule$balance[-n_months] * (1 + r))
  schedule$interest     <- schedule$balance_open - c(starting_cash, schedule$balance[-n_months])

  list(schedule                = schedule,
       bond_flows              = bond_flows[in_ladder, , drop = FALSE],
       overhang                = bond_flows[!in_ladder, , drop = FALSE],
       overhang_value          = if (nrow(bond_flows)) sum(bond_flows$amount[!in_ladder]) else 0,
       starting_cash           = starting_cash,
       solved_starting_cash    = solved_cash,
       min_balance             = min(schedule$balance),
       binding_month           = which.min(schedule$balance),
       n_months                = n_months,
       n_periods               = lia$n_periods,
       monthly_real_withdrawal = W,
       real_interest_rate      = real_interest_rate,
       monthly_real_rate       = r,
       total_inflow            = sum(schedule$bond_inflow),
       total_withdrawn         = sum(schedule$withdrawal),
       final_balance           = schedule$balance[n_months])
}

# Sleeve proportions -> units held, for testing a hand-picked ladder against the
# selector's answer:  units_j = weight_j * sleeve_value / real_price_j
ilb_units_from_weights <- function(weights, sleeve_value, bonds = ilb_fixed) {

  if (is.null(names(weights))) stop("weights must be a named vector of bond codes")
  unknown <- setdiff(names(weights), bonds$bond_code)
  if (length(unknown)) stop("unknown bond code(s): ", paste(unknown, collapse = ", "))
  if (any(weights < 0)) stop("weights must be non-negative (the ladder is long only)")

  out <- setNames(rep(0, nrow(bonds)), bonds$bond_code)
  out[names(weights)] <- weights
  out * sleeve_value / bonds$real_price
}

# ------------------------------------------------------------------------------
# 7. THE SELECTOR
# ------------------------------------------------------------------------------
# Arguments
#   ladder_years            length of the ladder, may be fractional
#   annual_real_withdrawal  real rands per year, LEVEL in real terms
#   total_pot_value,
#   withdrawal_rate_pct     the alternative input form; give one form or the other
#   nominal_interest_rate   the project's cash_annual_rate, DECIMAL
#   inflation_rate          the project's flat inflation assumption, DECIMAL.
#                           Used ONLY for the Fisher real rate, never to escalate
#                           the liability
#   start_date              must equal ILB_MODEL_START_DATE (prices and t0 are the
#                           same day); the argument exists so a caller states the
#                           date it thinks it is using and gets told if it is wrong
#   tie_break               which bond anchors a block when two ILBs redeem in the
#                           SAME MONTH
#   on_infeasible           what to do about a non-empty cash-only prefix
#
# TIE-BREAK - and an honest note about what the block change did to it
#
#   Ties are now SAME-MONTH ties, not same-year ties. Two ILBs only compete when
#   they redeem in the same month, which this universe never does; the rule only
#   bites on a synthetic or future universe.
#
#   Under the old annual buckets "earliest" genuinely saved cash: a bucket opened
#   in month 12(p-1)+1 and its bond could redeem up to eleven months into it, and
#   that lead time WAS the binding constraint. Blocks open ON the redemption
#   month, so the lead time is gone - and with it the argument. Under monthly
#   bucketing "earliest" and "latest" differ only in the day of the month, which
#   the schedule nets away inside the month, so on two otherwise-identical bonds
#   they return identical numbers. "cheapest" is now the only rule that can move
#   the answer, and it makes selection price-dependent, so a rebalance stops
#   being reproducible.
#
#   The default is "earliest": deterministic, price-independent, and it still
#   states the intent - take the money at the first opportunity - even though at
#   this granularity it costs nothing either way.
#
# on_infeasible - defaults to "flag", NOT "stop".
#
#   READ THIS BEFORE WIRING feasible INTO Dynamic_Ladder.R. It no longer means
#   what it meant under the annual pass. Blocks are cut at redemption months and
#   each block's bond funds every month up to the next redemption, so a maturity
#   gap is funded by the bond BEFORE it and there is no such thing as an unfunded
#   period any more. The only months no bond can reach are those before the FIRST
#   eligible redemption - nothing pays there at any price. Those are reported as
#   cash_only_months, and feasible is simply whether that set is empty.
#
#   On the SA universe at a 1 Aug 2026 start it is never empty: nothing redeems
#   before Mar 2028, so months 1-19 are always cash-only and feasible is always
#   FALSE. That is a fact about the market, not a failed solve. Do NOT hang a
#   "freeze this path" branch off it. Pass on_infeasible = "stop" only if you
#   genuinely want a cash-only prefix to be an error. STRUCTURAL problems - a bad
#   start_date, a broken workbook, no eligible bonds at all - always stop
#   regardless of this setting.
#
# Returns a list:
#   bond_units          named, aligned to the full universe (0 for bonds not held),
#                       in R100 of start-date-indexed par - feed to
#                       ilb_ladder_real_schedule()
#   bond_units_base_par the same holding expressed in rands of ORIGINAL par
#   bond_amounts        named, real rands invested per bond
#   cash_at_start       the solved minimum opening bank balance
#   total_cost          real rands spent on bonds
#   total_outlay        total_cost + cash_at_start, the full t0 cost
#   schedule            ilb_ladder_real_schedule() output, incl. running balance
#   min_balance         tightest closing balance (0 at the binding month, unless
#                        cash_buffer_months > 0, in which case it is the buffer)
#   solved_cash_at_start the unbuffered minimum C* the solve actually produced
#   cash_buffer         cash_at_start - solved_cash_at_start
#
# cash_buffer_months
#   The solve is EXACTLY binding by construction: C* is the SMALLEST opening
#   balance that keeps every closing balance non-negative, so min_balance is 0
#   at the binding month (month 78 on the base case) and 9 of the 96 months sit
#   below one month of income. That is fine in the selector's own world, where
#   the real rate credited on cash is the Fisher rate by assumption - and fatal
#   the moment the simulation credits a FIXED nominal rate against a STOCHASTIC
#   realised CPI, because the real rate then differs from the assumed one on
#   almost every path. Measured on the base case: the bootstrap delivers 5.78%
#   mean inflation against a 5% assumption, so the realised real cash rate is
#   -0.74% rather than 0.00%, and 86.5% of static ladders run the balance
#   negative at some point.
#
#   This parameter adds a stated margin - n months of the real monthly income -
#   on top of C*. It does not change the bond purchase at all, only the opening
#   cash. Sized on the base case: +1 month leaves 10.5% of paths short, +2
#   months leaves 0.0%, at a cost of 1.00% of the pot diverted from equity.
#   Default 0 so existing behaviour and every test in test_ilb_ladder() are
#   unchanged unless the caller asks for a buffer.
#   real_interest_rate  the Fisher-derived rate credited on cash
#   cash_only_months    months before the first redemption, funded from cash alone
#   blocks              per-block ledger; block 0 is the cash-only prefix
#   covered_blocks      blocks already in surplus from carry - nothing bought
#   allocation          one row per bond held, with the months it funds
#   liability, n_months, n_periods, n_blocks, ladder_years, eligible_bonds,
#   tie_break, start_date, feasible
optimize_ilb_ladder <- function(ladder_years,
                                annual_real_withdrawal = NULL,
                                total_pot_value        = NULL,
                                withdrawal_rate_pct    = NULL,
                                nominal_interest_rate  = 0.05,
                                inflation_rate         = 0.05,
                                start_date             = ILB_MODEL_START_DATE,
                                bonds                  = ilb_fixed,
                                cash_buffer_months     = 0,
                                tie_break              = c("earliest", "latest", "cheapest"),
                                on_infeasible          = c("flag", "stop")) {

  tie_break      <- match.arg(tie_break)
  on_infeasible  <- match.arg(on_infeasible)
  start_date     <- as.Date(start_date)
  bonds_defaulted <- missing(bonds)

  # The MODEL_START_DATE-style guard - but only when the caller is relying on
  # the DEFAULT `bonds = ilb_fixed`, i.e. the static day-0 table whose
  # real_price column is only valid on ILB_MODEL_START_DATE. There is no
  # valuation cell in the ILB workbook to read t0 from, so for that default
  # case the assertion runs the other way: the caller states the date it
  # believes it is pricing on, and is stopped if that is not the day the
  # default prices belong to.
  #
  # If the caller explicitly supplies their own `bonds` table (e.g. a
  # universe repriced off the real yield curve for an ANNUAL REVIEW/EXTENSION
  # at a later date - see Functions/ILB_Repricing.R), this guard is skipped:
  # real_price is just an input column here, never derived from start_date
  # internally, so a repriced table dated to today can be combined with a
  # start_date set to wherever the CURRENT ladder's coverage ends (the new
  # liability window's own month 1) with no conflict. The caller takes
  # responsibility for having repriced `bonds` correctly; this function only
  # ever prices off whatever is actually in the column.
  if (bonds_defaulted && !identical(start_date, ILB_MODEL_START_DATE)) {
    stop(sprintf(paste0("start_date (%s) is not the ILB model start date (%s) and no explicit `bonds` ",
                        "table was supplied. The default ilb_fixed prices in %s are struck on %s and ",
                        "time zero must be the same day for that default table - either supply a matching ",
                        "ILB extract, move ILB_MODEL_START_DATE deliberately, or pass a `bonds` table ",
                        "already repriced for start_date."),
                 format(start_date), format(ILB_MODEL_START_DATE), ILB_FILE,
                 format(ILB_MODEL_START_DATE)))
  }

  if (!is.finite(cash_buffer_months) || cash_buffer_months < 0) {
    stop("cash_buffer_months must be finite and non-negative (it is a number of MONTHS of income)")
  }

  real_interest_rate <- fisher_real_rate(nominal_interest_rate, inflation_rate)

  lia       <- ilb_liability(ladder_years, annual_real_withdrawal, total_pot_value, withdrawal_rate_pct)
  n_months  <- lia$n_months
  months_v  <- seq_len(n_months)
  W         <- lia$monthly_real_withdrawal
  w_vec     <- rep(W, n_months)                  # LEVEL in real terms

  # --- eligibility: priced, not yet redeemed, and redeems INSIDE the ladder ---
  # The last test is what makes the ladder self-liquidating: no bond it holds can
  # still be alive when the ladder ends, so there is nothing to mark to market
  # and sell. It is applied in MONTHS, so that a fractional ladder (say 10.5
  # years) cannot pick up a bond redeeming in the back half of its final,
  # part-length year.
  red_month  <- month_index_from_date_ilb(bonds$redemption_date, start_date)
  red_period <- year_period_from_date(bonds$redemption_date, start_date)
  eligible   <- is.finite(bonds$real_price) & bonds$real_price > 0 &
                bonds$redemption_date > start_date &
                red_month <= n_months

  if (!any(eligible)) {
    alive <- bonds$redemption_date > start_date
    stop(sprintf(paste0("no eligible ILBs: none of the %d bonds in the universe redeems inside a ",
                        "%.2f-year (%d-month) ladder. Shortest available maturity is %.2f years."),
                 nrow(bonds), ladder_years, n_months,
                 if (any(alive)) min(as.numeric(bonds$redemption_date[alive] - start_date)) / 365.25 else NA_real_))
  }

  # --- per-unit real cash flows, bucketed to months, for every eligible bond ---
  per_unit <- matrix(0, nrow = n_months, ncol = nrow(bonds),
                     dimnames = list(NULL, bonds$bond_code))
  for (j in which(eligible)) {
    cf <- single_ilb_real_cashflows(bonds$coupon_rate_real[j], bonds$redemption_amount_pct[j],
                                    bonds$redemption_date[j], start_date)
    if (!nrow(cf)) next
    mi   <- month_index_from_date_ilb(cf$date, start_date)
    keep <- mi <= n_months
    if (!any(keep)) next
    per_unit[, j] <- as.numeric(tapply(cf$amount[keep],
                                       factor(mi[keep], levels = months_v),
                                       sum, default = 0))
  }

  # --- block boundaries: one block per distinct redemption MONTH --------------
  # Block g runs from its anchor's redemption month to the month before the next
  # anchor redeems; the last block runs to the end of the ladder. So a block is
  # exactly as long as the maturity gap that follows its bond, and that bond is
  # bought big enough to cover all of it. That is the carry-back: a stretch with
  # no ILB redeeming in it is funded by the bond BEFORE it, whose money is
  # already in hand, rather than by day-zero cash.
  #
  # Cutting on the redemption month rather than on a calendar boundary is what
  # keeps the money invested until the month it is first needed. A block that
  # opened before its bond redeemed would have to be bridged by cash across the
  # lead time, and that bridge - not the maturity gaps - would then set
  # cash_at_start.
  anchor_months <- sort(unique(red_month[eligible]))
  n_blocks      <- length(anchor_months)
  block_start   <- anchor_months
  block_end     <- c(anchor_months[-1] - 1L, n_months)

  # Months before the first redemption. No bond in the universe pays there, so
  # nothing can fund them but the opening bank balance. This is the ONLY cash the
  # ladder structurally requires.
  cash_only_months <- if (anchor_months[1] > 1L) seq_len(anchor_months[1] - 1L) else integer(0)

  # --- one anchor per month; tie_break settles a shared month -----------------
  anchors  <- integer(n_blocks)
  n_cand_v <- integer(n_blocks)
  for (g in seq_len(n_blocks)) {
    m    <- anchor_months[g]
    cand <- which(eligible & red_month == m)
    n_cand_v[g] <- length(cand)
    anchors[g]  <- if (length(cand) == 1L) cand else switch(tie_break,
      earliest = cand[which.min(as.numeric(bonds$redemption_date[cand]))],
      latest   = cand[which.max(as.numeric(bonds$redemption_date[cand]))],
      cheapest = {
        pay <- per_unit[m, cand]
        bad <- !is.finite(pay) | pay <= 0
        if (any(bad)) {
          stop(sprintf(paste0("internal error: %s redeems in month %d but pays nothing into it - the ",
                              "cash-flow dates and the month bucketing have drifted apart."),
                       paste(bonds$bond_code[cand][bad], collapse = ", "), m))
        }
        cand[which.min(bonds$real_price[cand] / pay)]
      })
  }

  # --- the backward pass over blocks -----------------------------------------
  # Walking backwards is what makes each block's answer final: the bond bought
  # for block g redeems in the FIRST month of block g, so it pays nothing after
  # the block and can only ever reduce what the EARLIER blocks need. No block is
  # ever revisited.
  units  <- setNames(rep(0, nrow(bonds)), bonds$bond_code)
  inflow <- rep(0, n_months)                     # real inflows from bonds bought so far
  rows   <- vector("list", n_blocks)

  mk_row <- function(g, status, j, mb, need, have, funded, u, ncand) {
    data.frame(block           = g,
               status          = status,
               bond_code       = if (is.na(j)) NA_character_ else bonds$bond_code[j],
               redemption_date = if (is.na(j)) as.Date(NA)   else bonds$redemption_date[j],
               month_from      = min(mb),
               month_to        = max(mb),
               months_covered  = length(mb),
               withdrawals     = need,
               coupon_carry_in = have,
               funded_by_bond  = funded,
               net             = have + funded - need,
               units           = u,
               amount_invested = if (is.na(j)) 0 else u * bonds$real_price[j],
               n_candidates    = ncand,
               stringsAsFactors = FALSE)
  }

  for (g in n_blocks:1) {

    mb   <- block_start[g]:block_end[g]
    j    <- anchors[g]
    need <- sum(w_vec[mb])
    have <- sum(inflow[mb])                      # coupon carry from later-maturing bonds
    gap  <- need - have

    # Already in surplus on carry alone: solved, buy nothing. The surplus is not
    # clawed back - it stays in the bank account and helps the blocks before.
    if (gap <= 1e-9 * max(1, need)) {
      rows[[g]] <- mk_row(g, "covered_by_carry", j, mb, need, have, 0, 0, n_cand_v[g])
      next
    }

    own <- sum(per_unit[mb, j])                  # the anchor's flows inside its own block, per unit
    if (!is.finite(own) || own <= 0) {
      stop(sprintf(paste0("internal error: %s anchors block %d (months %d-%d) but contributes %.6g per ",
                          "unit to it - the month bucketing and the block boundaries have drifted apart."),
                   bonds$bond_code[j], g, min(mb), max(mb), own))
    }

    u <- gap / own                               # exact net-zero for this block
    units[j] <- u
    inflow   <- inflow + per_unit[, j] * u

    rows[[g]] <- mk_row(g, "funded", j, mb, need, have, u * own, u, n_cand_v[g])
  }

  blocks <- do.call(rbind, rows)

  # Block 0 is the cash-only prefix. Reported alongside the real blocks so the
  # ledger accounts for every month of the ladder exactly once.
  if (length(cash_only_months)) {
    blocks <- rbind(mk_row(0L, "cash_only", NA_integer_, cash_only_months,
                           sum(w_vec[cash_only_months]), sum(inflow[cash_only_months]),
                           0, 0, 0L),
                    blocks)
  }
  blocks <- blocks[order(blocks$block), , drop = FALSE]
  rownames(blocks) <- NULL

  covered_blocks <- blocks$block[blocks$status == "covered_by_carry"]
  feasible       <- length(cash_only_months) == 0L

  if (!feasible) {
    msg <- sprintf(paste0("months 1-%d are funded from opening cash alone: the first eligible ILB (%s) ",
                          "does not redeem until month %d (%s). Nothing in the universe pays before ",
                          "then, so that stretch cannot be bond-funded at any price - it is the ",
                          "irreducible cash floor of an ILB ladder started on %s, not a failed solve. ",
                          "Every month from %d onward is funded by a bond."),
                   max(cash_only_months), bonds$bond_code[anchors[1]], anchor_months[1],
                   format(start_date %m+% months(anchor_months[1] - 1L), "%b %Y"),
                   format(start_date), anchor_months[1])
    if (on_infeasible == "stop") stop("infeasible: ", msg) else warning(msg)
  }

  # --- assemble, aligned to the FULL universe ---------------------------------
  units[units < 1e-12] <- 0                      # scrub dust so ~0 holdings do not show up
  bond_amounts        <- units * bonds$real_price
  names(bond_amounts) <- bonds$bond_code
  # index_ratio_t0, NOT index_ratio: `bonds` may be a repriced universe whose
  # index_ratio is as at the review date, but a unit is R100 of T0-indexed par
  # by definition, so original par is always units * 100 / (the t0 ratio).
  ir_t0               <- if (!is.null(bonds$index_ratio_t0)) bonds$index_ratio_t0 else bonds$index_ratio
  base_par            <- ifelse(units > 0, units * 100 / ir_t0, 0)
  names(base_par)     <- bonds$bond_code
  total_cost          <- sum(bond_amounts)

  # Rebuilt from scratch rather than reusing the pass's own inflow vector - if
  # the two ever disagree, the bucketing conventions have drifted apart and the
  # self-test catches it.
  sched <- ilb_ladder_real_schedule(units, lia$annual_real_withdrawal, ladder_years,
                                    real_interest_rate, bonds = bonds, start_date = start_date)

  # The buffer sits on top of the SOLVED minimum, and the schedule is re-rolled
  # with it so balance/min_balance/final_balance all describe the account the
  # simulation will actually run. The bond side is untouched: a buffer buys time,
  # not cash flow.
  solved_cash_at_start <- sched$solved_starting_cash
  cash_buffer          <- cash_buffer_months * lia$monthly_real_withdrawal
  if (cash_buffer > 0) {
    sched <- ilb_ladder_real_schedule(units, lia$annual_real_withdrawal, ladder_years,
                                      real_interest_rate, bonds = bonds, start_date = start_date,
                                      starting_cash = solved_cash_at_start + cash_buffer)
  }

  held   <- which(units > 0)
  blk_of <- match(held, anchors)                 # which block each holding anchors
  allocation <- data.frame(
    bond_code         = bonds$bond_code[held],
    block             = blk_of,
    period            = red_period[held],               # calendar year it redeems in
    redemption_date   = bonds$redemption_date[held],
    funds_from        = block_start[blk_of],            # first month this bond funds
    funds_to          = block_end[blk_of],              # last month this bond funds
    years_to_maturity = as.numeric(bonds$redemption_date[held] - start_date) / 365.25,
    coupon_rate_real  = bonds$coupon_rate_real[held],
    index_ratio       = bonds$index_ratio[held],        # as at the pricing date of `bonds`
    index_ratio_t0    = ir_t0[held],                   # as at t0 - what `units` are struck off
    market_price      = bonds$market_price[held],       # nominal all-in, per R100 base par
    real_price        = bonds$real_price[held],         # per R100 indexed par
    units             = as.numeric(units[held]),        # R100 of indexed par
    base_par_amount   = as.numeric(base_par[held]),     # rands of ORIGINAL par - what a broker trades
    amount_invested   = as.numeric(bond_amounts[held]),
    weight            = if (total_cost > 0) as.numeric(bond_amounts[held]) / total_cost else 0,
    stringsAsFactors  = FALSE
  )
  allocation <- allocation[order(allocation$redemption_date), , drop = FALSE]
  rownames(allocation) <- NULL

  list(bond_units            = units,
       bond_units_base_par   = base_par,
       bond_amounts          = bond_amounts,
       cash_at_start         = sched$starting_cash,
       solved_cash_at_start  = solved_cash_at_start,
       cash_buffer           = cash_buffer,
       cash_buffer_months    = cash_buffer_months,
       total_cost            = total_cost,
       total_outlay          = total_cost + sched$starting_cash,
       schedule              = sched,
       min_balance           = sched$min_balance,
       real_interest_rate    = real_interest_rate,
       nominal_interest_rate = nominal_interest_rate,
       inflation_rate        = inflation_rate,
       cash_only_months      = cash_only_months,
       blocks                = blocks,
       covered_blocks        = covered_blocks,
       allocation            = allocation,
       liability             = lia,
       n_months              = n_months,
       n_periods             = lia$n_periods,
       n_blocks              = n_blocks,
       ladder_years          = ladder_years,
       eligible_bonds        = bonds$bond_code[eligible],
       tie_break             = tie_break,
       start_date            = start_date,
       feasible              = feasible)
}

# ------------------------------------------------------------------------------
# 8. CONSOLE REPORT
# ------------------------------------------------------------------------------
print_ilb_ladder <- function(res, digits = 2) {

  fmt <- function(x) formatC(ifelse(abs(x) < 0.5 * 10^(-digits), 0, x),
                             format = "f", big.mark = ",", digits = digits)

  cat("\n=================================================================\n")
  cat(sprintf(" ILB CASH-FLOW-MATCHED LADDER (REAL TERMS)  |  %.2f years (%d months, %d blocks)\n",
              res$ladder_years, res$n_months, res$n_blocks))
  cat("=================================================================\n")
  cat(sprintf(" Real income secured  : R %s p.a.  (R %s per month, level in real terms)\n",
              fmt(res$liability$annual_real_withdrawal), fmt(res$liability$monthly_real_withdrawal)))
  cat(sprintf(" Total real paid out  : R %s over the ladder\n", fmt(res$liability$total_withdrawn)))
  cat(sprintf(" Cash rate (Fisher)   : %.3f%% real  =  (1 + %.2f%% nominal)/(1 + %.2f%% inflation) - 1\n",
              100 * res$real_interest_rate, 100 * res$nominal_interest_rate, 100 * res$inflation_rate))
  cat("-----------------------------------------------------------------\n")
  cat(sprintf(" ILB PURCHASE COST    : R %s\n", fmt(res$total_cost)))
  cat(sprintf(" + opening cash       : R %s   (%.1f months of income)\n",
              fmt(res$cash_at_start), res$cash_at_start / res$liability$monthly_real_withdrawal))
  if (!is.null(res$cash_buffer) && res$cash_buffer > 0) {
    cat(sprintf("     of which solved  : R %s   (the binding minimum C*)\n", fmt(res$solved_cash_at_start)))
    cat(sprintf("     of which buffer  : R %s   (%.1f months, stated margin over C*)\n",
                fmt(res$cash_buffer), res$cash_buffer_months))
  }
  cat(sprintf(" = TOTAL t0 OUTLAY    : R %s   (cash is %.1f%% of it)\n",
              fmt(res$total_outlay), 100 * res$cash_at_start / res$total_outlay))
  cat(sprintf(" Bonds used           : %d of %d eligible\n",
              nrow(res$allocation), length(res$eligible_bonds)))
  cat(sprintf(" Tightest balance     : R %s  in month %d  (%s)\n",
              fmt(res$min_balance), res$schedule$binding_month,
              if (!is.null(res$cash_buffer) && res$cash_buffer > 0) "= the buffer; the solve binds beneath it"
              else "0 = the cash solve binds"))
  cat(sprintf(" Left over at the end : R %s\n", fmt(res$schedule$final_balance)))
  cat(sprintf(" Tie-break rule       : %s maturity within a shared redemption month\n", res$tie_break))
  if (length(res$cash_only_months)) {
    cat(sprintf(" Cash-only months     : 1-%d  (nothing redeems before month %d - irreducible)\n",
                max(res$cash_only_months), max(res$cash_only_months) + 1L))
  } else {
    cat(" Cash-only months     : none - a bond redeems in month 1\n")
  }
  if (length(res$covered_blocks)) {
    cat(sprintf(" Covered by carry     : block(s) %s  (already in surplus - nothing bought)\n",
                paste(res$covered_blocks, collapse = ", ")))
  }
  cat("-----------------------------------------------------------------\n")

  if (nrow(res$allocation)) {
    print(data.frame(block    = res$allocation$block,
                     bond     = res$allocation$bond_code,
                     redeems  = format(res$allocation$redemption_date),
                     funds    = sprintf("m%d-%d", res$allocation$funds_from, res$allocation$funds_to),
                     coupon   = sprintf("%.3f%%", res$allocation$coupon_rate_real),
                     ratio    = sprintf("%.3f", res$allocation$index_ratio),
                     real_px  = fmt(res$allocation$real_price),
                     units    = fmt(res$allocation$units),
                     base_par = fmt(res$allocation$base_par_amount),
                     invested = fmt(res$allocation$amount_invested),
                     weight   = sprintf("%6.2f%%", 100 * res$allocation$weight),
                     stringsAsFactors = FALSE),
          row.names = FALSE)
  } else {
    cat(" (empty portfolio)\n")
  }

  # Every month of the ladder appears in exactly one row. Block 0, when present,
  # is the cash-only prefix before the first redemption.
  cat("\n Block ledger (real rands):\n")
  bl <- res$blocks
  print(data.frame(block        = bl$block,
                   status       = bl$status,
                   bond         = ifelse(is.na(bl$bond_code), "-", bl$bond_code),
                   months       = sprintf("%d-%d", bl$month_from, bl$month_to),
                   n            = bl$months_covered,
                   withdrawals  = fmt(bl$withdrawals),
                   coupon_carry = fmt(bl$coupon_carry_in),
                   from_bond    = fmt(bl$funded_by_bond),
                   net          = fmt(bl$net),
                   stringsAsFactors = FALSE),
        row.names = FALSE)
  cat("=================================================================\n\n")

  invisible(res)
}

# ------------------------------------------------------------------------------
# 9. SELF-TESTS
# ------------------------------------------------------------------------------
# Nothing runs at source time except the data load and its guards, deliberately:
# sourcing this file should cost a workbook read and nothing else. Run the checks
# by hand with test_ilb_ladder().
test_ilb_ladder <- function(verbose = TRUE) {

  pass <- 0L; fail <- 0L
  chk <- function(name, ok, detail = "") {
    ok <- isTRUE(ok)
    if (ok) pass <<- pass + 1L else fail <<- fail + 1L
    if (verbose || !ok) cat(sprintf("  [%s] %s%s\n", if (ok) "PASS" else "FAIL", name,
                                    if (nzchar(detail)) paste0("  --  ", detail) else ""))
    invisible(ok)
  }
  near <- function(a, b, tol = 1e-6) all(abs(a - b) <= tol * pmax(1, abs(b)))

  cat("\n--- ILB ladder self-tests ---------------------------------------\n")

  # ---- Fisher ----
  cat("\n[Fisher]\n")
  chk("zero real rate when nominal == inflation", near(fisher_real_rate(0.05, 0.05), 0))
  r2 <- fisher_real_rate(0.08, 0.05)
  chk("Fisher identity (1+n) == (1+r)(1+i)", near((1 + r2) * 1.05, 1.08))
  chk("real rate can go negative", fisher_real_rate(0.03, 0.06) < 0)
  chk("percent-for-decimal slip is rejected",
      inherits(try(fisher_real_rate(5, 5), silent = TRUE), "try-error"))

  # ---- bucketing ----
  cat("\n[Calendar bucketing]\n")
  t0 <- ILB_MODEL_START_DATE
  chk("t0 itself is month 1 before the floor", months_elapsed_ilb(t0, t0) + 1L == 1L)
  chk("month 1 flows are floored to month 2",
      month_index_from_date_ilb(as.Date("2026-08-15"), t0) == 2L)
  chk("1 Sep 2026 is month 2", month_index_from_date_ilb(as.Date("2026-09-01"), t0) == 2L)
  chk("31 Aug 2026 is still month 1 pre-floor", months_elapsed_ilb(as.Date("2026-08-31"), t0) == 0L)
  chk("1 Aug 2027 is month 13", month_index_from_date_ilb(as.Date("2027-08-01"), t0) == 13L)
  chk("period 1 ends 31 Jul 2027", year_period_from_date(as.Date("2027-07-31"), t0) == 1L)
  chk("period 2 starts 1 Aug 2027", year_period_from_date(as.Date("2027-08-01"), t0) == 2L)
  d <- seq(t0 + 1, t0 + 4000, by = "13 days")
  chk("month and year buckets never disagree",
      all(year_period_from_date(d, t0) ==
          (month_index_from_date_ilb(d, t0, min_month = 1L) - 1L) %/% 12L + 1L))

  # ---- one bond's real cash flows ----
  cat("\n[Real cash flows]\n")
  b  <- ilb_fixed[ilb_fixed$bond_code == "I2029", ]
  cf <- single_ilb_real_cashflows(b$coupon_rate_real, b$redemption_amount_pct, b$redemption_date, t0)
  # Each date is redemption_date stepped back a whole number of 6-month jumps.
  # NOT the same day-of-month every time: I2029 redeems on 31 Mar, so %m-% lands
  # its September coupons on the 30th. That month-end clamping is the intended
  # behaviour and is why the dates are computed from redemption_date directly
  # rather than chained off one another.
  chk("coupons are semi-annual, stepped back from the redemption date",
      all(rev(cf$date) == (b$redemption_date %m-% months(seq(0, by = 6, length.out = nrow(cf))))))
  chk("consecutive coupons are one half-year apart",
      all(diff(as.numeric(cf$date)) %in% 180:186))
  chk("coupon size is coupon/2 per R100", near(cf$amount[1], b$coupon_rate_real / 2))
  chk("the last flow carries the redemption",
      near(cf$amount[nrow(cf)], 100 + b$coupon_rate_real / 2))
  chk("nothing is dated on or before t0", all(cf$date > t0))
  chk("an already-redeemed bond gives no flows",
      nrow(single_ilb_real_cashflows(5, 100, as.Date("2020-01-01"), t0)) == 0L)
  chk("real price is the market price deflated by the index ratio",
      near(ilb_fixed$real_price, ilb_fixed$market_price / ilb_fixed$index_ratio))
  chk("money agrees across the two par conventions",
      near((100 / ilb_fixed$index_ratio) / 100 * ilb_fixed$market_price, ilb_fixed$real_price))

  # ---- the base case ----
  cat("\n[Base case: 10y ladder, R500k real p.a., 5% nominal cash, 5% inflation]\n")
  res <- suppressWarnings(optimize_ilb_ladder(ladder_years = 10, annual_real_withdrawal = 500000,
                                              nominal_interest_rate = 0.05, inflation_rate = 0.05))
  s   <- res$schedule$schedule

  chk("no bond redeems after the ladder ends",
      all(month_index_from_date_ilb(ilb_fixed$redemption_date[res$bond_units > 0],
                                    res$start_date) <= res$n_months))
  chk("long only", all(res$bond_units >= 0))
  chk("holdings are named and aligned to the full universe",
      identical(names(res$bond_units), ilb_fixed$bond_code))
  chk("at most one bond bought per block", max(table(res$allocation$block)) <= 1)
  chk("every funded block nets to exactly zero",
      near(res$blocks$net[res$blocks$status == "funded"], 0, 1e-7))
  chk("nothing was bought into a block that was already solved",
      all(res$blocks$units[res$blocks$status != "funded"] == 0))

  # ---- block structure ----
  cat("\n[Block structure]\n")
  bl <- res$blocks[res$blocks$block > 0, ]
  chk("every block opens in the month its bond redeems",
      all(month_index_from_date_ilb(bl$redemption_date, t0) == bl$month_from))
  chk("blocks are contiguous - no month falls between two of them",
      all(bl$month_from[-1] == bl$month_to[-nrow(bl)] + 1L))
  chk("the last block runs to the end of the ladder",
      bl$month_to[nrow(bl)] == res$n_months)
  chk("blocks plus the cash-only prefix tile the ladder exactly",
      sum(res$blocks$months_covered) == res$n_months &&
      res$blocks$month_from[1] == 1L)
  chk("the cash-only prefix is exactly the months before the first redemption",
      identical(res$cash_only_months, seq_len(bl$month_from[1] - 1L)))
  chk("no eligible bond redeems inside the cash-only prefix",
      !any(month_index_from_date_ilb(ilb_fixed$redemption_date, t0) %in% res$cash_only_months &
           ilb_fixed$redemption_date > t0))
  # This is the carry-back working: the SA grid has nothing redeeming in several
  # whole years, and those years are covered by the bond in front of them rather
  # than by day-zero cash, so at least one block must be longer than a year.
  chk("a maturity gap is carried back onto an earlier bond",
      any(bl$months_covered > 12),
      sprintf("longest block covers %d months", max(bl$months_covered)))

  chk("bank balance never goes negative", min(s$balance) >= -1e-6,
      sprintf("min = %.6f", min(s$balance)))
  chk("the cash solve binds: min balance is 0", near(min(s$balance), 0, 1e-6))
  chk("cash_at_start >= one monthly instalment",
      res$cash_at_start >= res$liability$monthly_real_withdrawal - 1e-9)
  # At a zero real cash rate every block nets to zero exactly, so the balance
  # returns to its end-of-prefix level at every block boundary - and the cash
  # solve drives that level to zero. Nothing is left stranded at the end. Under
  # the annual buckets it was not: the intra-bucket lead time forced extra
  # opening cash that was then never spent.
  chk("nothing is stranded at ladder end (zero real cash rate)",
      near(s$balance[nrow(s)], 0, 1e-6),
      sprintf("final balance = %.2f", s$balance[nrow(s)]))
  chk("month 1 pays a withdrawal and receives nothing",
      s$bond_inflow[1] == 0 && near(s$withdrawal[1], res$liability$monthly_real_withdrawal))
  chk("withdrawals are level in real terms", near(diff(s$withdrawal), 0))
  chk("total_outlay == total_cost + cash_at_start",
      near(res$total_outlay, res$total_cost + res$cash_at_start))
  chk("amount invested == units * real price",
      near(sum(res$allocation$units * res$allocation$real_price), res$total_cost))
  chk("base par converts back to the same money",
      near(sum(res$allocation$base_par_amount / 100 * res$allocation$market_price), res$total_cost))
  chk("self-liquidating: no cash flows fall outside the ladder",
      res$schedule$overhang_value == 0)

  # independent replay of the bank account, month by month
  r_m    <- res$schedule$monthly_real_rate
  bal    <- res$cash_at_start
  replay <- numeric(nrow(s))
  for (m in seq_len(nrow(s))) {
    bal       <- bal - s$withdrawal[m] + s$bond_inflow[m]
    replay[m] <- bal
    bal       <- bal * (1 + r_m)
  }
  chk("closed-form balance matches a month-by-month replay", near(replay, s$balance, 1e-6))
  chk("cash in + inflows - withdrawals + interest == final balance",
      near(res$cash_at_start + sum(s$bond_inflow) - sum(s$withdrawal) + sum(s$interest),
           s$balance[nrow(s)], 1e-6))

  # ---- input forms, and invariance ----
  cat("\n[Input forms and invariance]\n")
  res_rate <- suppressWarnings(optimize_ilb_ladder(ladder_years = 10, total_pot_value = 10000000,
                                                   withdrawal_rate_pct = 5,
                                                   nominal_interest_rate = 0.05, inflation_rate = 0.05))
  chk("(pot, rate) and (amount) forms agree",
      near(res_rate$total_cost, res$total_cost) && near(res_rate$cash_at_start, res$cash_at_start))
  chk("both forms at once is rejected",
      inherits(try(optimize_ilb_ladder(10, 500000, total_pot_value = 1e7,
                                       withdrawal_rate_pct = 5), silent = TRUE), "try-error"))
  chk("neither form is rejected",
      inherits(try(optimize_ilb_ladder(10), silent = TRUE), "try-error"))

  res2 <- suppressWarnings(optimize_ilb_ladder(ladder_years = 10, annual_real_withdrawal = 1000000,
                                               nominal_interest_rate = 0.05, inflation_rate = 0.05))
  chk("bond cost scales linearly with the withdrawal", near(res2$total_cost, 2 * res$total_cost))
  chk("cash_at_start scales linearly too", near(res2$cash_at_start, 2 * res$cash_at_start))
  chk("the same bonds are chosen at either size",
      identical(res2$allocation$bond_code, res$allocation$bond_code))

  res_hi <- suppressWarnings(optimize_ilb_ladder(ladder_years = 10, annual_real_withdrawal = 500000,
                                                 nominal_interest_rate = 0.09, inflation_rate = 0.05))
  chk("a higher real cash rate buys the same bonds",
      identical(res_hi$allocation$bond_code, res$allocation$bond_code))
  chk("a higher real cash rate needs less opening cash",
      res_hi$cash_at_start < res$cash_at_start)
  chk("the cash rate does not move the bond cost", near(res_hi$total_cost, res$total_cost))

  # ---- carry-back: the whole point of cutting blocks at redemption months ----
  # Rebuilding the OLD annual-bucket pass here would be half the file, so the
  # property is tested directly instead: every month from the first redemption
  # onward must be inside a block whose bond has already redeemed, which is
  # exactly what "no month waits for money that has not arrived" means, and is
  # what the annual pass could not deliver.
  cat("\n[Carry-back]\n")
  first_red <- min(month_index_from_date_ilb(
    ilb_fixed$redemption_date[ilb_fixed$redemption_date > t0], t0))
  bond_funded <- unlist(Map(seq, bl$month_from, bl$month_to))
  chk("every month from the first redemption on is inside a block",
      identical(sort(bond_funded), first_red:res$n_months))
  chk("no month is funded by a bond that has not yet redeemed",
      all(month_index_from_date_ilb(bl$redemption_date, t0) <= bl$month_from))
  chk("cash only ever funds months no bond can reach",
      length(res$cash_only_months) == 0L || max(res$cash_only_months) < first_red)

  # ---- tie-break ----
  cat("\n[Tie-break]\n")
  chk("no ties to break in this universe: every ILB redeems in its own month",
      max(table(month_index_from_date_ilb(
        ilb_fixed$redemption_date[ilb_fixed$redemption_date > t0], t0))) == 1)
  res_l <- suppressWarnings(optimize_ilb_ladder(ladder_years = 10, annual_real_withdrawal = 500000,
                                                nominal_interest_rate = 0.05, inflation_rate = 0.05,
                                                tie_break = "latest"))
  chk("with no ties, the rule does not change the answer",
      identical(res_l$allocation$bond_code, res$allocation$bond_code) &&
      near(res_l$cash_at_start, res$cash_at_start))

  # Because the real universe never ties, all three branches would otherwise be
  # dead code. Force a tie with a synthetic bond redeeming in the SAME MONTH as
  # R210 - ties are same-month now, not same-year - and check each rule picks
  # what it claims to.
  syn   <- ilb_fixed[ilb_fixed$bond_code %in% c("R210", "I2029"), ]
  extra <- syn[1, ]
  extra$bond_code        <- "TEST2"
  extra$redemption_date  <- as.Date("2028-03-15")     # same month as R210's 31 Mar 2028
  extra$coupon_rate_real <- 8
  extra$market_price     <- 120
  extra$index_ratio      <- 1
  extra$base_cpi         <- ILB_REFERENCE_CPI
  extra$real_price       <- 120                        # deliberately dear, so "cheapest" avoids it
  syn   <- rbind(syn, extra)
  pick_with <- function(tb) {
    r <- suppressWarnings(optimize_ilb_ladder(3, 500000, bonds = syn, tie_break = tb))
    r$blocks$bond_code[r$blocks$block == 1]
  }
  chk("the synthetic pair really does share a month",
      month_index_from_date_ilb(as.Date("2028-03-15"), t0) ==
      month_index_from_date_ilb(as.Date("2028-03-31"), t0))
  chk("'earliest' takes the earlier maturity in the month", identical(pick_with("earliest"), "TEST2"))
  chk("'latest' takes the later maturity in the month",     identical(pick_with("latest"),   "R210"))
  chk("'cheapest' takes the cheaper cash flow",             identical(pick_with("cheapest"), "R210"))

  tb_ear <- suppressWarnings(optimize_ilb_ladder(3, 500000, bonds = syn, tie_break = "earliest"))
  tb_lat <- suppressWarnings(optimize_ilb_ladder(3, 500000, bonds = syn, tie_break = "latest"))
  chk("the tie-break cannot move a block boundary",
      identical(tb_ear$blocks$month_from, tb_lat$blocks$month_from) &&
      identical(tb_ear$blocks$month_to,   tb_lat$blocks$month_to))
  chk("both tie-breaks still fund every block to zero",
      near(tb_ear$blocks$net[tb_ear$blocks$status == "funded"], 0, 1e-7) &&
      near(tb_lat$blocks$net[tb_lat$blocks$status == "funded"], 0, 1e-7))

  # Day-of-month is netted away inside the month, so on a pair identical in every
  # other respect "earliest" and "latest" must return the SAME numbers. This is
  # the claim the tie-break note in section 7 makes, tested rather than asserted:
  # under the old annual buckets the two rules differed by up to eleven months of
  # opening cash, and cutting blocks at redemption months is what removed that.
  twin <- ilb_fixed[ilb_fixed$bond_code %in% c("R210", "I2029"), ]
  tw   <- twin[twin$bond_code == "R210", ]
  tw$bond_code       <- "R210B"
  tw$redemption_date <- as.Date("2028-03-15")
  twin <- rbind(twin, tw)
  tw_e <- suppressWarnings(optimize_ilb_ladder(3, 500000, bonds = twin, tie_break = "earliest"))
  tw_l <- suppressWarnings(optimize_ilb_ladder(3, 500000, bonds = twin, tie_break = "latest"))
  chk("on identical twins the two rules pick different bonds",
      !identical(tw_e$allocation$bond_code, tw_l$allocation$bond_code))
  chk("...but return identical cost and cash - the day of the month nets away",
      near(tw_e$total_cost, tw_l$total_cost) && near(tw_e$cash_at_start, tw_l$cash_at_start),
      sprintf("earliest %.4f / %.4f vs latest %.4f / %.4f",
              tw_e$total_cost, tw_e$cash_at_start, tw_l$total_cost, tw_l$cash_at_start))

  # ---- guards ----
  cat("\n[Guards]\n")
  chk("a wrong start_date stops",
      inherits(try(optimize_ilb_ladder(10, 500000, start_date = as.Date("2026-07-31")),
                   silent = TRUE), "try-error"))
  chk("on_infeasible = 'stop' turns a cash-only prefix into an error",
      inherits(try(optimize_ilb_ladder(10, 500000, on_infeasible = "stop"), silent = TRUE),
               "try-error"))
  chk("a cash-only prefix warns under the default",
      inherits(tryCatch(optimize_ilb_ladder(10, 500000), warning = function(w) w), "warning"))
  chk("feasible is FALSE only because of that prefix - the blocks all solved",
      !res$feasible && length(res$cash_only_months) > 0 &&
      all(res$blocks$net[res$blocks$block > 0] >= -1e-7))
  chk("a ladder shorter than the shortest bond stops",
      inherits(try(optimize_ilb_ladder(1, 500000), silent = TRUE), "try-error"))
  chk("unnamed holdings are rejected",
      inherits(try(ilb_ladder_real_schedule(c(1, 2), 500000, 10, 0), silent = TRUE), "try-error"))
  chk("unknown bond codes are rejected",
      inherits(try(ilb_ladder_real_schedule(c(NOPE = 1), 500000, 10, 0), silent = TRUE), "try-error"))
  chk("negative holdings are rejected",
      inherits(try(ilb_ladder_real_schedule(c(I2029 = -1), 500000, 10, 0), silent = TRUE), "try-error"))
  chk("a negative ladder length is rejected",
      inherits(try(optimize_ilb_ladder(-1, 500000), silent = TRUE), "try-error"))

  # ---- fractional ladder ----
  cat("\n[Fractional ladder]\n")
  res_f <- suppressWarnings(optimize_ilb_ladder(ladder_years = 7.5, annual_real_withdrawal = 500000,
                                                nominal_interest_rate = 0.05, inflation_rate = 0.05))
  chk("7.5 years is 90 months over 8 periods", res_f$n_months == 90L && res_f$n_periods == 8L)
  chk("no bond redeems past month 90",
      all(month_index_from_date_ilb(ilb_fixed$redemption_date[res_f$bond_units > 0], t0) <= 90L))
  chk("funded blocks still net to zero on a part-year ladder",
      near(res_f$blocks$net[res_f$blocks$status == "funded"], 0, 1e-7))
  chk("the last block still runs to month 90",
      max(res_f$blocks$month_to) == 90L)
  chk("blocks still tile a part-year ladder exactly",
      sum(res_f$blocks$months_covered) == res_f$n_months)
  chk("balance stays non-negative on a fractional ladder",
      min(res_f$schedule$schedule$balance) >= -1e-6)

  # ---- the result the change was made for ----
  # Cash was 51.8% of t0 outlay under calendar-year buckets with no carry-back.
  # These are regression bounds, not targets: they fail loudly if a future edit
  # quietly reintroduces the lead-time bridge or drops the carry-back.
  cat("\n[Cash share]\n")
  for (L in c(5, 10, 15)) {
    rL <- suppressWarnings(optimize_ilb_ladder(ladder_years = L, total_pot_value = 10000000,
                                               withdrawal_rate_pct = 5))
    share <- rL$cash_at_start / rL$total_outlay
    chk(sprintf("%2dy ladder holds under a third of t0 outlay in cash", L), share < 1 / 3,
        sprintf("cash %.1f%% of R%.0f outlay", 100 * share, rL$total_outlay))
  }

  cat(sprintf("\n--- %d passed, %d failed ----------------------------------------\n\n", pass, fail))
  invisible(list(pass = pass, fail = fail))
}

#test_ilb_ladder()
#res <- optimize_ilb_ladder(ladder_years = 15, annual_real_withdrawal = 400000,
                            #nominal_interest_rate = 0.05, inflation_rate = 0.05)
#print_ilb_ladder(res)
