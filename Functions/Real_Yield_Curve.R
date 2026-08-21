# ==============================================================================
# REAL YIELD CURVE: nominal PCA curve minus stochastic breakeven inflation
# ==============================================================================
# Real curve(t, path) = Nominal curve(t, path) - Breakeven inflation(t, path).
#
# The nominal curve (Functions/yield_curve_simulation.R, simulate_yield_curves())
# is used exactly as built by the team - not modified here, called as-is via
# its own get_curve(month, sim) accessor.
#
# Breakeven inflation is NOT a flat constant subtracted off every curve - it is
# simulated as its own mean-reverting stochastic process (Ornstein-Uhlenbeck,
# monthly-discretised, i.e. an AR(1) in continuous-time form), centered on a
# long-run mean of 4.5%, so it varies by month and by simulation path just like
# the nominal curve does.
#
# ------------------------------------------------------------------------------
# ASSUMED / UNVALIDATED PARAMETERS - not calibrated to real market data.
# Exposed as named arguments (defaults below) specifically so they can be
# swapped out for sensitivity testing once real breakeven data is available:
#   theta (long-run mean, %)        4.5   - stated in the brief
#   kappa (mean-reversion speed)    0.15  - placeholder: ~half-life of ~4.6
#                                            years (log(2)/0.15), a plausible
#                                            order of magnitude for inflation
#                                            expectations, NOT fitted
#   sigma (annualised volatility, %) 1.0  - placeholder: order-of-magnitude
#                                            guess for SA breakeven vol, NOT
#                                            fitted
# ------------------------------------------------------------------------------
#
# SIMPLIFICATION also worth flagging: breakeven inflation here is a single
# scalar per (month, sim), subtracted flat across every tenor. A real
# breakeven curve has its own term structure (short vs long breakevens differ);
# that is not modelled - every tenor point on a given (month, sim)'s nominal
# curve gets the same breakeven subtracted.
# ==============================================================================

# ------------------------------------------------------------------------------
# simulate_breakeven_inflation()
# ------------------------------------------------------------------------------
# Monthly Euler discretisation of dX_t = kappa*(theta - X_t)*dt + sigma*dW_t,
# dt = 1/12, all rates in PERCENTAGE POINTS (matching simulate_yield_curves()'s
# output units, e.g. 7.75 means 7.75%, not 0.0775) so the two can be subtracted
# directly without a unit-conversion bug.
#
# Returns a [horizon_months x n_sims] matrix, one path per column - same shape
# convention as inflation_monthly_ratios / cum_inflation elsewhere in the model.
simulate_breakeven_inflation <- function(horizon_months,
                                          n_sims,
                                          theta = 4.5,
                                          kappa = 0.15,
                                          sigma = 1.0,
                                          x0    = theta,
                                          seed  = 4501) {
  dt <- 1 / 12
  set.seed(seed)
  be <- matrix(NA_real_, nrow = horizon_months, ncol = n_sims)
  for (s in seq_len(n_sims)) {
    prev <- x0
    for (t in seq_len(horizon_months)) {
      cur <- prev + kappa * (theta - prev) * dt + sigma * sqrt(dt) * rnorm(1)
      be[t, s] <- cur
      prev <- cur
    }
  }
  be
}

# ------------------------------------------------------------------------------
# build_real_yield_curve()
# ------------------------------------------------------------------------------
# yc            : the list returned by simulate_yield_curves() (nominal curve
#                 model, used as-is).
# breakeven_sim : [horizon_months x n_sims] matrix from
#                 simulate_breakeven_inflation() - must match yc's
#                 horizon_months / n_sims.
#
# Returns a list with get_real_curve(month, sim), mirroring yc$get_curve()'s
# signature and output shape exactly (same named tenor vector, same units) so
# it can be dropped into the repricing step in place of the nominal accessor.
build_real_yield_curve <- function(yc, breakeven_sim) {
  if (nrow(breakeven_sim) != yc$horizon_months || ncol(breakeven_sim) != yc$n_sims) {
    stop(sprintf(
      "breakeven_sim is %d x %d but yc expects %d x %d (horizon_months x n_sims).",
      nrow(breakeven_sim), ncol(breakeven_sim), yc$horizon_months, yc$n_sims))
  }

  get_real_curve <- function(month, sim, include_residual = TRUE) {
    yc$get_curve(month, sim, include_residual = include_residual) - breakeven_sim[month, sim]
  }

  list(
    get_real_curve = get_real_curve,
    breakeven_sim  = breakeven_sim,
    tenors_months  = yc$tenors_months,
    horizon_months = yc$horizon_months,
    n_sims         = yc$n_sims
  )
}
