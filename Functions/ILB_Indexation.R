# ==============================================================================
# ILB INDEXATION: Reference CPI, Index Ratios, and the ONE real -> nominal bridge
# ==============================================================================
# Turns the existing CPI moving-block bootstrap output (Functions/
# moving_block_bootstrap.R, invoked in Thesis.R as part of the shared
# equity/bond/CPI bootstrap - see inflation_monthly_ratios there) into the
# quantities the ILB ladder needs:
#
#   Reference CPI(t, path) - the absolute CPI index level implied by that
#     path's bootstrapped monthly growth ratios, anchored to the last actual
#     (non-bootstrapped) CPI Index value.
#   Index Ratio(t, path)   - Reference CPI(t, path) / Base CPI, per bond. The
#     market's own quoting convention. Used for PRICES and PAR, never for
#     converting the selector's money into nominal money - see below.
#   real_to_nominal_factor(t, path) - Reference CPI(t, path) / CPI(t0). The
#     ONLY sanctioned way to turn a rand amount produced by Functions/
#     Bond Selection ILB.R into a rand amount the nominal engine can spend,
#     bank or pay out.
#
# This file does NOT bootstrap or reload CPI itself. Everything below derives
# from the single CPI series already loaded and bootstrapped in Thesis.R
# (CPI_Index_Long_Format.xlsx -> cpi_values -> the "Inf_Growth" column fed into
# moving_block_bootstrap()), so path-level Reference CPI cannot drift from
# anything else driven by that same series.
#
# ------------------------------------------------------------------------------
# WHY real_to_nominal_factor() EXISTS - the defect it closes
# ------------------------------------------------------------------------------
# Bond Selection ILB.R measures money in ONE UNIT = R100 of START-DATE-INDEXED
# par. Under that convention a bond's quoted real flows (R(coupon/2), R100) are
# already denominated in T0 RANDS, because the par they are struck off has
# already been grossed up by the t0 index ratio. Concretely, for a holding of
# `units`:
#
#     base par            = units * 100 / index_ratio(t0)
#     nominal flow at t   = base par / 100 * quoted_flow * index_ratio(t)
#                         = units * quoted_flow * index_ratio(t)/index_ratio(t0)
#                         = units * quoted_flow * CPI(t)/CPI(t0)
#
# So the conversion factor for the SELECTOR'S OWN MONEY is CPI(t)/CPI(t0) -
# bond-independent - and NOT the bond's index ratio CPI(t)/base_cpi. Multiplying
# by the index ratio instead over-states every converted amount by that bond's
# t0 index ratio, which on this universe runs 1.138 (I2043/I2058) to 3.188
# (R202). That is exactly what this file and Functions/ILB_Repricing.R used to
# do, in three separate places: it inflated every coupon and redemption credited
# to the cash account, and marked the ladder at ~1.80x its own purchase cost at
# t0 itself. Functions/Invariants.R's boundary checks pin both down.
#
# The index ratio is still the right number for PRICES quoted per R100 of BASE
# par (reprice_universe()'s market_price column) and for converting units to
# original par. It is never the right number for converting the selector's rand
# amounts. Keep the two jobs apart.
#
# ------------------------------------------------------------------------------
# MONTH CONVENTION - stated once, used everywhere
# ------------------------------------------------------------------------------
# Two indexings coexist in this project and the relationship between them is
# fixed:
#
#   elapsed month k    (0-based)   the INSTANT t0 %m+% months(k). k = 0 IS t0.
#                                  This is Functions/ILB_Repricing.R's `month`.
#   simulation month m (1-based)   the SPAN [t0 + (m-1) months, t0 + m months).
#                                  This is Dynamic_Ladder.R's loop counter and
#                                  Bond Selection ILB.R's schedule$month.
#
#                          k = m - 1
#
# A month's VALUATION INSTANT is its start, so everything that happens "in
# simulation month m" - the annuity-due withdrawal, the annual review, an
# extension purchase, a coupon credited - converts at the factor for elapsed
# month m-1. reference_cpi's row k is the price level k months after t0, and
# row 0 is CPI(t0) itself, which the matrix does not physically have - hence
# nominal_factor_matrix() below prepends it as a literal 1.
#
# Cash flows are therefore indexed at the START of the month they land in rather
# than on the day they land, i.e. up to a month early. That is deliberate and,
# if anything, closer to the market than the alternative: SA ILBs index off a
# 4-month-LAGGED, daily-interpolated CPI, so the true reference level for a flow
# is earlier still (see the SIMPLIFICATION note in single_ilb_real_cashflows()).
# One convention applied everywhere is worth more here than sub-monthly
# precision the rest of the model does not have.
# ==============================================================================

# ------------------------------------------------------------------------------
# reference_cpi_path()
# ------------------------------------------------------------------------------
# inflation_monthly_ratios : [horizon x n_sims] matrix of bootstrapped monthly
#                            CPI growth ratios - Thesis.R's
#                            inflation_monthly_ratios (= sim_paths[, "Inf_Growth", ]).
# cpi_anchor               : scalar - the last ACTUAL (non-bootstrapped) CPI
#                            Index value, i.e. Thesis.R's
#                            cpi_values[length(cpi_values)]. This is the same
#                            real-world CPI print already used as input to the
#                            bootstrap, not a separately sourced number.
#
# Returns a [horizon x n_sims] matrix of absolute Reference CPI index levels.
# Row k is the price level k months after t0; there is no row 0.
reference_cpi_path <- function(inflation_monthly_ratios, cpi_anchor) {
  cpi_anchor * apply(inflation_monthly_ratios, 2, cumprod)
}

# ------------------------------------------------------------------------------
# assert_cpi_anchor()
# ------------------------------------------------------------------------------
# The bootstrapped CPI path is anchored to the last actual CPI print; the ILB
# workbook's index ratios are struck off ILB_REFERENCE_CPI. real_to_nominal_
# factor() divides one by the other, so if they are not the SAME price level the
# factor is silently mis-scaled by their ratio from month 1 onward - every
# deposit, every mark, every extension purchase, on every path.
#
# They agree today (both 107.5 - the Jun 2026 print). This stops the run rather
# than warning if a future data refresh moves one without the other: a warning
# in a 1,000-path run is not visible enough to protect a number that multiplies
# into everything.
assert_cpi_anchor <- function(cpi_anchor, cpi_t0 = ILB_REFERENCE_CPI, tol = 1e-6) {
  if (!is.finite(cpi_anchor) || cpi_anchor <= 0) {
    stop("assert_cpi_anchor(): cpi_anchor must be finite and positive, got ", cpi_anchor)
  }
  if (abs(cpi_anchor / cpi_t0 - 1) > tol) {
    stop(sprintf(paste0("CPI anchor mismatch: the bootstrapped CPI path is anchored to %.4f (the last ",
                        "actual CPI print) but the ILB workbook's index ratios are struck off ",
                        "ILB_REFERENCE_CPI = %.4f. These must be the same price level, or every ",
                        "real -> nominal conversion is mis-scaled by %.4fx from month 1 onward. ",
                        "Refresh the workbook's index ratios onto the new print, or move ",
                        "ILB_REFERENCE_CPI - deliberately, either way."),
                 cpi_anchor, cpi_t0, cpi_anchor / cpi_t0))
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# nominal_factor_matrix()
# ------------------------------------------------------------------------------
# The whole real -> nominal bridge, precomputed: a [(horizon + 1) x n_sims]
# matrix whose ROW k+1 is the factor for ELAPSED MONTH k (see the month
# convention above). Row 1 is elapsed month 0 = t0 = a literal 1, which makes
# the identity "a real rand at t0 IS a nominal rand at t0" explicit rather than
# assumed.
#
# Build this ONCE in Thesis.R and pass it down. It is the same object
# Dynamic_Ladder.R's old `cum_inflation` was (reference_cpi is that cumprod
# scaled by the anchor, and the anchor equals cpi_t0 - assert_cpi_anchor()
# enforces exactly that), so the two are now ONE object rather than two that
# happen to agree. Deleting the duplicate removes a drift channel.
nominal_factor_matrix <- function(reference_cpi, cpi_t0 = ILB_REFERENCE_CPI) {
  rbind(rep(1, ncol(reference_cpi)), reference_cpi / cpi_t0)
}

# ------------------------------------------------------------------------------
# real_to_nominal_factor()
# ------------------------------------------------------------------------------
# Accessor for the same thing when the matrix is not to hand.
#   elapsed_months : 0 = t0, k = k months after t0. Vectorised.
#   sim            : one path (scalar).
# Returns CPI(t0 + k months, sim) / CPI(t0), with k = 0 giving exactly 1.
real_to_nominal_factor <- function(elapsed_months, sim, reference_cpi,
                                   cpi_t0 = ILB_REFERENCE_CPI) {
  k <- as.integer(elapsed_months)
  if (any(k < 0L)) stop("real_to_nominal_factor(): elapsed_months must be >= 0 (0 = t0)")
  if (any(k > nrow(reference_cpi))) {
    stop(sprintf("real_to_nominal_factor(): elapsed month %d is past the end of reference_cpi (%d rows)",
                 max(k), nrow(reference_cpi)))
  }
  ifelse(k == 0L, 1, reference_cpi[pmax(k, 1L), sim] / cpi_t0)
}

# ------------------------------------------------------------------------------
# index_ratio_matrix() / index_ratio_for_bond()
# ------------------------------------------------------------------------------
# The MARKET's index ratio, Reference CPI(t, path) / Base CPI - same [horizon x
# n_sims] shape as reference_cpi. This is the right number for a price quoted
# per R100 of BASE par, and (via the FROZEN t0 ratio, ilb_fixed$index_ratio_t0)
# for converting units to original par. It is NOT the conversion factor for the
# selector's rand amounts - use real_to_nominal_factor() for those. See header.
index_ratio_matrix <- function(reference_cpi, base_cpi) {
  reference_cpi / base_cpi
}

index_ratio_for_bond <- function(reference_cpi, bond_code, bonds) {
  base_cpi <- bonds$base_cpi[bonds$bond_code == bond_code]
  if (length(base_cpi) != 1) {
    stop(sprintf("bond_code '%s' not found (or not unique) in bonds.", bond_code))
  }
  index_ratio_matrix(reference_cpi, base_cpi)
}

# ------------------------------------------------------------------------------
# nominal_bond_deposits()
# ------------------------------------------------------------------------------
# One bond's contractual real cash flows, per unit, translated to nominal
# deposits for ONE simulation path and bucketed into the GLOBAL simulation month
# numbering (1..horizon relative to global_valuation_date = ILB_MODEL_START_DATE)
# that deposits_by_month uses throughout run_dynamic_ladder_simulation() -
# regardless of when the bond was actually bought.
#
# This is a different job from Functions/ILB_Repricing.R's
# reprice_bond_real_price(): that marks REMAINING value to a (possibly moved)
# real yield curve for valuation; this is the actual rand amount that lands when
# a coupon or redemption is paid - fixed by contract, no yield curve involved.
#
# purchase_date         the date this bond was actually bought (global t0 for
#                       the initial ladder; a later review date for an
#                       extension) - controls WHICH cash flows exist at all
#                       (single_ilb_real_cashflows() only returns flows after
#                       this date).
# global_valuation_date always ILB_MODEL_START_DATE - controls how those flows'
#                       dates convert into month INDICES, so a bond bought later
#                       still lands in the right global month, not month 1 of
#                       its own purchase.
# reference_cpi_col     reference_cpi[, sim] - one path's Reference CPI column.
#
# CONVERSION: quoted real flow * CPI(start of its simulation month)/CPI(t0).
# Note what is NOT an argument any more: base_cpi. This function used to take it
# and multiply by the bond's index ratio, over-stating every deposit by that
# bond's t0 index ratio (1.14x - 3.19x). Functions/Invariants.R's
# boundary_check_deposits_vs_schedule() is the regression test: with inflation
# switched off, these deposits must reproduce the selector's own real schedule
# exactly.
#
# Returns a numeric vector, length `horizon`: nominal deposit in each simulation
# month (0 where nothing is paid).
nominal_bond_deposits <- function(coupon_rate_real, redemption_amount_pct,
                                  redemption_date, purchase_date, global_valuation_date,
                                  reference_cpi_col, horizon, cpi_t0 = ILB_REFERENCE_CPI) {
  cf  <- single_ilb_real_cashflows(coupon_rate_real, redemption_amount_pct,
                                   redemption_date, purchase_date)
  out <- rep(0, horizon)
  if (nrow(cf) == 0) return(out)

  mi   <- month_index_from_date_ilb(cf$date, global_valuation_date)
  keep <- mi >= 1 & mi <= horizon
  if (!any(keep)) return(out)

  # simulation month m converts at elapsed month m - 1 (the start of the month)
  m_keep <- mi[keep]
  k      <- m_keep - 1L
  factor <- ifelse(k == 0L, 1, reference_cpi_col[pmax(k, 1L)] / cpi_t0)

  nominal_amt <- cf$amount[keep] * factor

  for (j in seq_along(nominal_amt)) {
    out[m_keep[j]] <- out[m_keep[j]] + nominal_amt[j]
  }
  out
}

# ------------------------------------------------------------------------------
# real_bond_deposits()
# ------------------------------------------------------------------------------
# The same schedule with the conversion left off: the bond's contractual flows
# in T0 REAL rands, per unit, bucketed the same way. Not used by the engine's
# cash mechanics - it exists so Functions/Invariants.R can hold the nominal
# schedule to account month by month (nominal must equal real * factor, always)
# and so the reporting layer can deflate without re-deriving anything.
real_bond_deposits <- function(coupon_rate_real, redemption_amount_pct,
                               redemption_date, purchase_date, global_valuation_date,
                               horizon) {
  cf  <- single_ilb_real_cashflows(coupon_rate_real, redemption_amount_pct,
                                   redemption_date, purchase_date)
  out <- rep(0, horizon)
  if (nrow(cf) == 0) return(out)

  mi   <- month_index_from_date_ilb(cf$date, global_valuation_date)
  keep <- mi >= 1 & mi <= horizon
  if (!any(keep)) return(out)

  m_keep <- mi[keep]
  amt    <- cf$amount[keep]
  for (j in seq_along(amt)) {
    out[m_keep[j]] <- out[m_keep[j]] + amt[j]
  }
  out
}

# ------------------------------------------------------------------------------
# nominal_deposits_for_holdings() / real_deposits_for_holdings()
# ------------------------------------------------------------------------------
# Sum the per-unit schedules across a set of bond_units (only bond_units > 0
# contribute), for one simulation path. Called once per path over the full
# initial holding at simulation setup (purchase_date = global_valuation_date for
# everything), and again, per path, for just the INCREMENTAL units bought on an
# extension (purchase_date = that review's date) - ADDED on top of, never
# replacing, whatever is already scheduled.
nominal_deposits_for_holdings <- function(bond_units, bonds, purchase_date,
                                          global_valuation_date, reference_cpi_col, horizon,
                                          cpi_t0 = ILB_REFERENCE_CPI) {
  held <- names(bond_units)[bond_units > 0]
  out  <- rep(0, horizon)
  if (length(held) == 0) return(out)

  rows <- bonds[bonds$bond_code %in% held, , drop = FALSE]
  for (i in seq_len(nrow(rows))) {
    per_unit <- nominal_bond_deposits(rows$coupon_rate_real[i], rows$redemption_amount_pct[i],
                                      rows$redemption_date[i], purchase_date, global_valuation_date,
                                      reference_cpi_col, horizon, cpi_t0)
    out <- out + per_unit * as.numeric(bond_units[rows$bond_code[i]])
  }
  out
}

real_deposits_for_holdings <- function(bond_units, bonds, purchase_date,
                                       global_valuation_date, horizon) {
  held <- names(bond_units)[bond_units > 0]
  out  <- rep(0, horizon)
  if (length(held) == 0) return(out)

  rows <- bonds[bonds$bond_code %in% held, , drop = FALSE]
  for (i in seq_len(nrow(rows))) {
    per_unit <- real_bond_deposits(rows$coupon_rate_real[i], rows$redemption_amount_pct[i],
                                   rows$redemption_date[i], purchase_date, global_valuation_date,
                                   horizon)
    out <- out + per_unit * as.numeric(bond_units[rows$bond_code[i]])
  }
  out
}
