# ==============================================================================
# ILB LADDER REPRICING AT FUTURE DATES
# ==============================================================================
# For each future date, for each path, for each bond:
#   1. Reprice the bond's REAL price off the real yield curve (Functions/
#      Real_Yield_Curve.R) at the bond's own then-remaining term.
#   2. Convert to nominal rands with real_to_nominal_factor() (Functions/
#      ILB_Indexation.R).
#
# REWRITE NOTE: this file originally called Bond Selection.R's
# single_bond_cashflows() and used bonds_fixed's nominal column names. Both were
# deleted in favour of the real-terms selector, Functions/Bond Selection ILB.R,
# and this file was rewritten against that.
#
# ------------------------------------------------------------------------------
# THE UNITS FIX - read this before changing anything below
# ------------------------------------------------------------------------------
# There are TWO different "per what" conventions in play and this file used to
# mix them:
#
#   per R100 of BASE par     the market's quoting convention. real price times
#                            the bond's INDEX RATIO (CPI(t)/base_cpi) gives the
#                            nominal price. This is what market_price means, at
#                            t0 and at every future date.
#
#   per UNIT                 Bond Selection ILB.R's convention, and the one
#                            `bond_units` / `bond_holdings` are counted in:
#                            R100 of T0-INDEXED par. Because the par has already
#                            been grossed up by the t0 index ratio, a unit's
#                            real flows and real price are denominated in T0
#                            RANDS, and the conversion to nominal is
#                            CPI(t)/CPI(t0) - NOT the index ratio.
#
# reprice_ladder() marks HOLDINGS, which are counted in units, so it must use
# CPI(t)/CPI(t0). It used to multiply by the index ratio instead, over-stating
# the mark by the bond's own t0 index ratio - 1.138x (I2043/I2058) to 3.188x
# (R202), and ~1.80x on the base-case ladder as a whole, AT T0 ITSELF, before a
# single month of inflation. That inflated funded ratios (over-extension), and
# inflated the lump sum credited to equity when a ladder liquidates.
# Functions/Invariants.R's boundary_check_reprice_t0() is the regression test:
# reprice_ladder(month = 0) must equal the selector's own total_cost, exactly.
#
# The same distinction governs extend_ilb_ladder(): the selector prices and
# solves in T0-REAL rands, so its bond_cost and cash_at_start must be converted
# before they touch the nominal EPort or the nominal cash account. They now are,
# and the real amounts are returned alongside so the audit layer can see both.
#
# ------------------------------------------------------------------------------
# MONTH CONVENTION
# ------------------------------------------------------------------------------
# `month` here is the ELAPSED-MONTH index k defined in Functions/
# ILB_Indexation.R: the INSTANT t0 %m+% months(k).
#
#   month == 0  -> t0 itself. Uses ilb_fixed's own real_price column directly
#                  and a conversion factor of exactly 1, so t0 falls out of the
#                  same formula as every other date rather than being a special
#                  case.
#   month 1..H  -> repriced via the real curve, converted at CPI(t)/CPI(t0).
#                  reference_cpi's row k and get_real_curve()'s row k are both
#                  "k months after t0", so the three modules share one indexing
#                  with no re-mapping.
#
# A caller working in SIMULATION months m (1-based spans, Dynamic_Ladder.R's
# loop counter) reviews at the START of month m and must therefore pass
# month = m - 1. That is the k = m - 1 relationship, applied at the call site
# rather than hidden here, so this file keeps one unambiguous convention.
#
# Calls (does NOT modify) functions from Functions/Bond Selection ILB.R and
# Functions/ILB_Indexation.R, sourced elsewhere in the project:
#   single_ilb_real_cashflows()  - one bond's remaining real cash flows
#   real_to_nominal_factor()     - the real -> nominal bridge
#   get_real_curve()             - returned by build_real_yield_curve()
#   reference_cpi (matrix)       - built by reference_cpi_path()
# ==============================================================================

if (!require("lubridate")) install.packages("lubridate")
library(lubridate)

# ------------------------------------------------------------------------------
# real_curve_yield()
# ------------------------------------------------------------------------------
# Interpolates one (month, sim)'s curve (a named tenor vector, tenor labels
# keyed in MONTHS via tenors_months) at arbitrary terms t_years. Clamped linear
# interpolation.
#
# GENERIC DESPITE THE NAME: nothing about this is real-specific, and ARVA.R and
# compute_funded_ratio() both call it with NOMINAL curve vectors. The name is a
# historical accident; the behaviour is correct for either.
real_curve_yield <- function(t_years, real_curve_vec, tenors_months) {
  t_months <- pmin(pmax(t_years * 12, min(tenors_months)), max(tenors_months))
  approx(tenors_months, as.numeric(real_curve_vec), xout = t_months)$y
}

# ------------------------------------------------------------------------------
# reprice_bond_real_price()
# ------------------------------------------------------------------------------
# One bond, one (month, sim): PV of its remaining real cash flows discounted at
# that (month, sim)'s real curve, at each cash flow's own remaining term (not
# one flat rate for the whole bond).
#
# The result reads two ways, and they are the same number:
#   - the REAL price per R100 of BASE par (the market convention), and
#   - the T0-REAL price of ONE UNIT (R100 of t0-indexed par).
# The first times the index ratio is the nominal price per R100 base par; the
# second times CPI(t)/CPI(t0) is the nominal value of a unit. Which one you want
# depends entirely on what you are counting - see the units note in the header.
#
# Because the PV sums every remaining flow it is already dirty/all-in - no
# separate "clean price minus accrued interest" step. Returns 0 for a bond that
# has already fully redeemed (single_ilb_real_cashflows() returns zero rows).
reprice_bond_real_price <- function(coupon_rate_real, redemption_amount_pct,
                                    redemption_date, valuation_date_now,
                                    real_curve_vec, tenors_months) {
  cf <- single_ilb_real_cashflows(coupon_rate_real, redemption_amount_pct,
                                  redemption_date, valuation_date_now)
  if (nrow(cf) == 0) return(0)
  y <- real_curve_yield(cf$t_years, real_curve_vec, tenors_months) / 100
  sum(cf$amount / (1 + y)^cf$t_years)
}

# ------------------------------------------------------------------------------
# reprice_universe()
# ------------------------------------------------------------------------------
# Reprices EVERY bond in `bonds` (not just held ones) at one (month, sim),
# returning an ilb_fixed-SHAPED data frame that can be passed straight into
# optimize_ilb_ladder(bonds = ...) for an annual-review extension purchase.
#
# Columns and what each one means AFTER repricing:
#   real_price             T0-REAL price of one unit == real price per R100 base
#                          par. This is the column optimize_ilb_ladder() spends,
#                          and it is in the same T0-REAL money as the
#                          annual_real_withdrawal it is solving against - which
#                          is why the solve stays internally consistent and only
#                          its OUTPUT needs converting.
#   index_ratio            the MARKET's ratio as at this date, CPI(t)/base_cpi.
#                          Overwritten deliberately: it is an as-at-a-date
#                          quantity.
#   index_ratio_t0         carried through UNTOUCHED from `bonds`. Units are
#                          struck off the t0 ratio and never re-based, so this
#                          is what converts units back to original par.
#   market_price           nominal all-in price per R100 BASE par
#                          (= real_price * index_ratio). Reporting only.
#   nominal_price_per_unit nominal cost of ONE UNIT today
#                          (= real_price * CPI(t)/CPI(t0)). This is what a unit
#                          bought today actually costs the nominal engine.
#
# month == 0 returns `bonds` unchanged - t0 prices are already correct, and
# real_price is already the nominal cost per unit there (factor 1).
# Bonds already matured get real_price = 0 (hence market_price = 0), which
# optimize_ilb_ladder()'s own eligibility filter excludes - no extra filtering.
reprice_universe <- function(bonds, valuation_date, month, sim, real_yc, reference_cpi,
                             cpi_t0 = ILB_REFERENCE_CPI) {
  if (month == 0) {
    out <- bonds
    if (is.null(out$index_ratio_t0)) out$index_ratio_t0 <- out$index_ratio
    out$nominal_price_per_unit <- out$real_price
    return(out)
  }

  valuation_date_now <- valuation_date %m+% months(month)
  real_curve_vec     <- real_yc$get_real_curve(month = month, sim = sim)

  real_price <- vapply(seq_len(nrow(bonds)), function(i) {
    reprice_bond_real_price(bonds$coupon_rate_real[i], bonds$redemption_amount_pct[i],
                            bonds$redemption_date[i], valuation_date_now,
                            real_curve_vec, real_yc$tenors_months)
  }, numeric(1))

  f <- real_to_nominal_factor(month, sim, reference_cpi, cpi_t0)

  out <- bonds
  if (is.null(out$index_ratio_t0)) out$index_ratio_t0 <- out$index_ratio
  out$real_price             <- real_price
  out$index_ratio            <- reference_cpi[month, sim] / bonds$base_cpi
  out$market_price           <- real_price * out$index_ratio
  out$nominal_price_per_unit <- real_price * f
  out
}

# ------------------------------------------------------------------------------
# reprice_ladder()
# ------------------------------------------------------------------------------
# Marks currently-HELD bonds (bond_units > 0) at one (month, sim) - the direct
# replacement for the old Redington-era mark_to_model_bond_value(), used to feed
# compute_funded_ratio()'s bond_value and to value the ladder when it liquidates
# into equity.
#
# Arguments:
#   bond_units     named numeric vector, aligned to bonds$bond_code (0 = not
#                  held) - same convention optimize_ilb_ladder() returns. Counted
#                  in UNITS (R100 of t0-indexed par).
#   bonds          ilb_fixed, or a table with the same columns.
#   valuation_date the model's t0 (ILB_MODEL_START_DATE).
#   month          ELAPSED months after t0; 0 = t0. See the header - a caller
#                  working in simulation months m must pass m - 1.
#   sim            which simulated path - must be a valid column of BOTH real_yc
#                  and reference_cpi; the two must have been simulated with the
#                  SAME n_sims/horizon or this silently reads misaligned paths
#                  against each other.
#   real_yc        list from build_real_yield_curve().
#   reference_cpi  [horizon x n_sims] matrix from reference_cpi_path().
#
# Returns one row per held bond:
#   real_price   t0-real price per unit
#   real_value   units * real_price          - the holding in T0 RANDS
#   factor       CPI(t)/CPI(t0)
#   index_ratio  the market's ratio at this date (reporting; NOT used to value)
#   rand_value   real_value * factor         - the holding in NOMINAL rands
#
# At month 0 factor is exactly 1 and real_price is ilb_fixed's own column, so
# sum(rand_value) == sum(real_value) == the selector's total_cost. That identity
# is asserted before every run by Functions/Invariants.R.
reprice_ladder <- function(bond_units, bonds, valuation_date, month, sim,
                           real_yc, reference_cpi, cpi_t0 = ILB_REFERENCE_CPI) {

  held <- names(bond_units)[bond_units > 0]
  if (length(held) == 0) {
    return(data.frame(bond_code = character(0), real_price = numeric(0),
                      real_value = numeric(0), factor = numeric(0),
                      index_ratio = numeric(0), rand_value = numeric(0)))
  }

  rows  <- bonds[bonds$bond_code %in% held, , drop = FALSE]
  units <- as.numeric(bond_units[rows$bond_code])

  if (month == 0) {
    real_price  <- rows$real_price
    index_ratio <- rows$index_ratio
    f           <- 1
  } else {
    valuation_date_now <- valuation_date %m+% months(month)
    real_curve_vec     <- real_yc$get_real_curve(month = month, sim = sim)

    real_price <- vapply(seq_len(nrow(rows)), function(i) {
      reprice_bond_real_price(rows$coupon_rate_real[i], rows$redemption_amount_pct[i],
                              rows$redemption_date[i], valuation_date_now,
                              real_curve_vec, real_yc$tenors_months)
    }, numeric(1))

    index_ratio <- reference_cpi[month, sim] / rows$base_cpi
    f           <- real_to_nominal_factor(month, sim, reference_cpi, cpi_t0)
  }

  real_value <- units * real_price

  data.frame(
    bond_code   = rows$bond_code,
    real_price  = real_price,
    real_value  = real_value,
    factor      = f,
    index_ratio = index_ratio,
    rand_value  = real_value * f,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# extend_ilb_ladder()
# ------------------------------------------------------------------------------
# The annual-review "Extend & Harvest" purchase: buy what is needed to move the
# ladder's target from wherever it currently reaches out to a NEW target
# (target_years, measured in years from `valuation_date`, same convention as
# Dynamic_Ladder.R's ladder_years_current) - additively. Never sells or resizes
# anything already held except by TOPPING UP the one bond currently anchoring
# the ladder's last, still-open block.
#
# Why a top-up rather than a fresh mini-ladder from today: the currently-held
# bond with the latest redemption date (the "anchor") pays its entire remaining
# value as ONE lump sum, on its own redemption date. Buying MORE of that same
# bond, today, at today's price, lets that same lump sum stretch further into the
# future, covering more months, right up until a genuinely new bond becomes
# eligible to redeem and take over. That is strictly more capital-efficient than
# starting a fresh sub-ladder at the boundary date, which would treat the anchor
# as already-matured (wrongly - it is still buyable today) and demand an
# unnecessary cash top-up instead.
#
# This works across a SEQUENCE of yearly extensions with no lookahead, because
# each call only asks "given what is held RIGHT NOW, and THIS year's target, what
# is the incremental purchase". The anchor is re-derived from current holdings
# every call, never tracked as state.
#
# ---- REAL IN, BOTH OUT -------------------------------------------------------
# optimize_ilb_ladder() lives entirely in T0-REAL rands: it is handed
# annual_real_withdrawal (a t0-real level) and prices in real_price (t0-real per
# unit), and it returns t0-real money. The engine that consumes the answer is
# NOMINAL. Converting at the boundary - here - is what keeps the solve internally
# consistent while still charging the nominal portfolio the right amount.
#
# This is the second place the old code spent real rands as if they were nominal:
# bond_cost and cash_at_start went straight onto EPort and the cash account with
# no conversion, which made every extension cheaper than it was, by a factor that
# grew with cumulative inflation (~1.4x by year 10 at 5%). Both real and nominal
# amounts are returned so Functions/Invariants.R can check the pair.
#
# Arguments:
#   bond_units_held          named numeric vector, this path's CURRENT holdings
#                            in units (must have at least one bond held - for the
#                            very first purchase call optimize_ilb_ladder()).
#   target_years             the ladder's new total target length, in years from
#                            `valuation_date` (== new_ladder_years, NOT
#                            years-remaining).
#   valuation_date           the model's t0 (ILB_MODEL_START_DATE).
#   month, sim               today's ELAPSED month (0 = t0) and path.
#   annual_real_withdrawal   THIS year's withdrawal level, in T0-REAL rands.
#                            Re-solving the whole open block at this level (not
#                            just the incremental months) means an income change
#                            is picked up for the anchor's already-open block
#                            too - deliberate, not an approximation.
#   bonds                    ilb_fixed (or same-shaped table) - the FULL
#                            universe, unrepriced; repriced here.
#   cash_buffer_months       stated margin, in months of real income, added to
#                            the sub-solve's own binding minimum C*. The engine
#                            leaves this at 0 and buffers only the INITIAL
#                            ladder: run_dynamic_ladder_simulation() sizes each
#                            extension against the cash the pooled account is
#                            actually projected to hold (see below), so a
#                            per-extension buffer would be margin stacked on top
#                            of margin that is already there.
#
# ---- WHY THIS FUNCTION NO LONGER RETURNS A SPENDABLE CASH FIGURE -------------
# It used to return cash_top_up = fresh$cash_at_start (converted), and the engine
# charged that to equity on every extension. That is the FULL opening balance the
# sub-ladder needs at its own start date - not an increment - so an anchor that
# stayed put for four consecutive years had the same bridge funded four times.
# Measured on a 25-path run: sim 1 paid R155k, R165k, R179k and R186k of REAL
# rands into the account across months 12, 24, 36 and 48, all solving from the
# same anchor (R202, redeeming month 88), and ended the horizon with R1.25m of
# today's money parked in cash at a negative real rate instead of compounding in
# equity. Across 25 paths the double-charging totalled R31.8m nominal.
#
# The shortfall cannot be computed here: it depends on what the pooled cash
# account will actually hold on the sub-solve's start date, which is engine
# state. So this function now returns the REQUIREMENT and the date it applies on,
# and run_dynamic_ladder_simulation() projects its own account forward and funds
# only the gap. The spendable-but-wrong figure is deliberately gone rather than
# deprecated - leaving it in the return list is what let it be spent.
#
# Returns:
#   incremental_units   named, aligned to `bonds` - units to ADD to holdings.
#   bond_cost_real      T0-REAL rands of bonds bought this call.
#   bond_cost           NOMINAL rands of bonds bought (the one figure here that
#                       IS directly spendable - it is a genuine increment).
#   factor              CPI(t)/CPI(t0) at `month`, used to convert bond_cost.
#   cash_required_real  T0-REAL opening balance the sub-ladder needs ON
#                       solve_start_date. A REQUIREMENT, not a payment.
#   solve_start_date    the date that requirement applies on (anchor_date - 1).
#   solve_start_month   the simulation month (1-based) containing it, so the
#                       caller can project its own account to the right point.
#   anchor_bond_code    which currently-held bond was topped up.
#   fresh               the full optimize_ilb_ladder() result, kept for
#                       diagnostics - NOT the thing to read total_cost or
#                       bond_units off directly (it double-counts the anchor's
#                       already-held portion), and its money is REAL.
extend_ilb_ladder <- function(bond_units_held, target_years, valuation_date, month, sim,
                              annual_real_withdrawal, bonds, real_yc, reference_cpi,
                              nominal_interest_rate = 0.05, inflation_rate = 0.05,
                              cash_buffer_months = 0,
                              cpi_t0 = ILB_REFERENCE_CPI,
                              tie_break = c("earliest", "latest", "cheapest"),
                              on_infeasible = c("flag", "stop")) {

  held_codes <- names(bond_units_held)[bond_units_held > 0]
  if (length(held_codes) == 0) {
    stop("extend_ilb_ladder(): bond_units_held has nothing held - this is an initial ",
         "purchase, not an extension. Call optimize_ilb_ladder() directly instead.")
  }

  held_rows   <- bonds[bonds$bond_code %in% held_codes, , drop = FALSE]
  anchor_idx  <- which.max(held_rows$redemption_date)
  anchor_code <- held_rows$bond_code[anchor_idx]
  anchor_date <- held_rows$redemption_date[anchor_idx]

  # optimize_ilb_ladder()'s eligibility test is redemption_date > start_date
  # (strict). Setting start_date to the anchor's OWN redemption date makes it
  # fail its own test - from that exact vantage point it looks already-spent,
  # even though we are buying MORE of it today, before it redeems. Backing
  # start_date off by one day keeps the anchor eligible without pulling in any of
  # its EARLIER coupons (the next one back is ~6 months earlier, well before this
  # shifted date) and - since none of the ILBs in this universe redeem on the 1st
  # of a month - without shifting any bond's month bucket either.
  solve_start_date <- anchor_date - 1

  target_end_date <- valuation_date %m+% months(round(target_years * 12))
  years_to_solve  <- as.numeric(target_end_date - solve_start_date) / 365.25
  if (years_to_solve <= 0) {
    stop(sprintf(paste0("extend_ilb_ladder(): new target (%s) is not past the current anchor ",
                        "bond %s's own redemption date (%s) - nothing to extend."),
                 format(target_end_date), anchor_code, format(anchor_date)))
  }

  repriced <- reprice_universe(bonds, valuation_date, month, sim, real_yc, reference_cpi, cpi_t0)

  fresh <- optimize_ilb_ladder(
    ladder_years           = years_to_solve,
    annual_real_withdrawal = annual_real_withdrawal,
    nominal_interest_rate  = nominal_interest_rate,
    inflation_rate         = inflation_rate,
    start_date             = solve_start_date,
    bonds                  = repriced,
    cash_buffer_months     = cash_buffer_months,
    tie_break              = tie_break,
    on_infeasible          = on_infeasible
  )

  # `fresh` is a sub-ladder scoped to [solve_start_date, target_end_date] - every
  # OTHER held bond redeems strictly before solve_start_date (the anchor is by
  # definition the latest-redeeming held bond), so none of them are eligible in
  # `fresh` at all - they are out of scope, not "reduced to zero". Comparing the
  # FULL held vector against fresh$bond_units would misread "not part of this
  # sub-problem" as "sell this back"; restricting to fresh$eligible_bonds keeps
  # the comparison to the anchor (top-up) and any genuinely new bonds.
  held_aligned <- setNames(rep(0, nrow(bonds)), bonds$bond_code)
  held_aligned[names(bond_units_held)] <- bond_units_held

  incremental_units <- setNames(rep(0, nrow(bonds)), bonds$bond_code)
  in_scope <- names(incremental_units) %in% fresh$eligible_bonds
  incremental_units[in_scope] <- fresh$bond_units[in_scope] - held_aligned[in_scope]

  if (any(incremental_units < -1e-6)) {
    warning("extend_ilb_ladder(): fresh solve needs FEWER units of some already-held, in-scope ",
            "bond(s) than currently held (",
            paste(names(incremental_units)[incremental_units < -1e-6], collapse = ", "),
            ") - this should not happen under the additive-only design; investigate rather than ",
            "silently clip. Clipping to 0 for now.")
  }
  incremental_units[incremental_units < 0] <- 0

  # --- the real -> nominal boundary, crossed exactly once, here ---------------
  # Only the BOND cost is converted and returned as spendable. The cash
  # requirement stays in real terms and travels with the date it applies on, for
  # the caller to net against its own projected balance - see the header.
  f              <- real_to_nominal_factor(month, sim, reference_cpi, cpi_t0)
  bond_cost_real <- sum(incremental_units * repriced$real_price)

  list(
    incremental_units  = incremental_units,
    bond_cost_real     = bond_cost_real,
    bond_cost          = bond_cost_real * f,
    factor             = f,
    cash_required_real = fresh$cash_at_start,
    solve_start_date   = solve_start_date,
    solve_start_month  = month_index_from_date_ilb(solve_start_date, valuation_date),
    anchor_bond_code   = anchor_code,
    fresh              = fresh
  )
}

# ==============================================================================
# TODO / PLACEHOLDER: This output format is NOT final.
# It needs to be integrated with the decision matrix once that component is
# ready. Confirm the expected input shape with the team before building anything
# downstream that depends on this table.
# ==============================================================================
#
# build_repricing_table()
# ------------------------------------------------------------------------------
# Runs reprice_ladder() over every requested (month, sim) pair and stacks the
# results into one tidy/long data.frame: one row per (path, date, bond). Purely a
# storage/shape exercise - it decides nothing and feeds nothing.
#
#   months, sims   integer vectors of which ELAPSED months (0 = t0) and which
#                  paths to run. NOT defaulted to the full grid deliberately:
#                  that is well over a million reprice_ladder() calls.
#
# Returns: sim, month, date, bond_code, real_price, real_value, factor,
# index_ratio, rand_value.
build_repricing_table <- function(bond_units, bonds, valuation_date,
                                  months, sims, real_yc, reference_cpi,
                                  cpi_t0 = ILB_REFERENCE_CPI) {
  rows <- vector("list", length(months) * length(sims))
  idx <- 1L
  for (m in months) {
    for (s in sims) {
      res <- reprice_ladder(bond_units, bonds, valuation_date, m, s, real_yc, reference_cpi, cpi_t0)
      if (nrow(res) == 0) next
      res$sim   <- s
      res$month <- m
      res$date  <- valuation_date %m+% months(m)
      rows[[idx]] <- res
      idx <- idx + 1L
    }
  }
  out <- do.call(rbind, rows[seq_len(idx - 1L)])
  out <- out[, c("sim", "month", "date", "bond_code", "real_price", "real_value",
                 "factor", "index_ratio", "rand_value")]
  rownames(out) <- NULL
  out
}
