###############################################################
# Dynamic Bond Ladder - Helper Functions
###############################################################

# ------------------------------------------------------------------------
# Funded ratio: total assets (equity + cash + current bond ladder value)
# over the present value of ALL future expected retirement expenses -
# every withdrawal from now until the end of the retiree's expected
# lifetime (max_age), not just the years remaining on the CURRENT bond
# ladder target. A funded_ratio >= 1 means total assets can support every
# future expense, not merely "can fund the ladder to its own end".
#
# expected_annual_expense is today's currently-secured withdrawal
# (current_income) - the retiree's real, parameter-defined spending level
# right now. Each future year's expense is no longer held flat in nominal
# terms: it is grown forward from expected_annual_expense at inflation_rate,
# so the liability reflects the real purchasing power the retiree will
# actually need in year t, not today's rand amount repeated 30 times. This
# mirrors the same cumulative-CPI indexation applied to the actual monthly
# cash withdrawal in run_dynamic_ladder_simulation(), so a path whose real
# spending is rising with inflation shows a correspondingly larger
# liability - the funded ratio can only fall due to genuine erosion of real
# purchasing power, not sit artificially flat while nominal costs climb.
# inflation_rate defaults to 0 (old flat-expense behaviour) so any other
# caller of this function that hasn't been updated to pass it - e.g. the
# duplicated toggle wrapper in Sensitivity_Analysis.R - keeps working
# unchanged.
# ------------------------------------------------------------------------
compute_funded_ratio <- function(equity_value, cash_value, bond_value,
                                 expected_annual_expense, horizon_years,
                                 inflation_rate = 0) {

  t_vec <- 1:horizon_years
  y_vec <- curve_yield(t_vec) / 100
  inflated_expense_t <- expected_annual_expense * (1 + inflation_rate)^t_vec
  pv_t  <- inflated_expense_t * (1 + y_vec)^(-t_vec)
  cost_to_fully_fund <- sum(pv_t)

  total_assets <- equity_value + cash_value + bond_value

  list(
    funded_ratio       = total_assets / cost_to_fully_fund,
    cost_to_fully_fund = cost_to_fully_fund,
    total_assets       = total_assets
  )
}

# ------------------------------------------------------------------------
# Re-derive the bond ladder AND the cash buffer together for a new
# (remaining_years, annual_withdrawal) target, as of valuation_date_now,
# and return the equity harvest (positive = pull from equity) or release
# (negative = excess flows back to equity) needed to true the portfolio
# up to that target from its current bond + cash position.
#
# optimize_redington_immunization() is written around (total_pot_value,
# withdrawal_rate_pct) because that's how the retiree originally specifies
# the ladder ("5% of a R10m pot"). Here we already know the exact annual
# withdrawal amount, so total_pot_value = annual_withdrawal and
# withdrawal_rate_pct = 100 reduces calculate_liability_metrics()'s
# `total_pot_value * withdrawal_rate_pct/100` back down to exactly
# annual_withdrawal - same liability, cleaner call.
# ------------------------------------------------------------------------
rebalance_ladder <- function(remaining_years, annual_withdrawal, valuation_date_now,
                             current_bond_value, current_cash_value,
                             convexity_buffer, cash_annual_rate,
                             bond_metrics_precomputed = NULL,
                             inflation_rate = 0, inflation_factors = NULL) {

  res <- optimize_redington_immunization(
    total_pot_value      = annual_withdrawal,
    withdrawal_rate_pct  = 100,
    ladder_years         = remaining_years,
    convexity_buffer     = convexity_buffer,
    valuation_date       = valuation_date_now,
    on_infeasible        = "flag",
    bond_metrics_precomputed = bond_metrics_precomputed
  )

  if (!isTRUE(res$feasible)) {
    return(list(feasible = FALSE, message = res$message))
  }

  new_bond_cost <- res$total_cost

  new_cf <- bond_cashflow_schedule(res$portfolio_allocation, bonds_fixed, valuation_date_now)
  new_buffer <- size_ladder_cash_buffer(
    bond_cf           = new_cf,
    ladder_years      = remaining_years,
    annual_withdrawal = annual_withdrawal,
    cash_annual_rate  = cash_annual_rate,
    inflation_rate    = inflation_rate,
    inflation_factors = inflation_factors
  )
  new_C0 <- new_buffer$C0

  harvest_needed <- (new_bond_cost + new_C0) - (current_bond_value + current_cash_value)

  list(
    feasible        = TRUE,
    duration_matched = res$duration_matched,
    bond_units      = res$bond_units,
    bond_cf         = new_cf,
    new_bond_cost   = new_bond_cost,
    new_C0          = new_C0,
    harvest_needed  = harvest_needed,
    cash_buffer_obj = new_buffer
  )
}

# ------------------------------------------------------------------------
# Mark-to-model value of a set of current bond holdings (named vector,
# bond_code -> units) at a given valuation date, using the same
# calculate_bond_metrics()$pv used everywhere else in Reddington.R -
# there is no separate "future market price" model, so PV under
# curve_yield() stands in for market value at any date other than the
# original MTM snapshot (this is a known placeholder limitation, see
# Reddington.R's curve_yield() comments).
# ------------------------------------------------------------------------
mark_to_model_bond_value <- function(bond_units_held, valuation_date_now, bond_metrics_precomputed = NULL) {
  if (sum(bond_units_held) <= 0) return(0)
  bm <- if (!is.null(bond_metrics_precomputed)) bond_metrics_precomputed else calculate_bond_metrics(bonds_fixed, valuation_date_now)
  matched <- bm$pv[match(names(bond_units_held), bm$bond_code)]
  sum(bond_units_held * matched, na.rm = TRUE)
}

###############################################################
# Main simulation: dynamic ladder phase + ARVA phase, all paths,
# all months, in one loop. Replaces the old static Phase 1 / Phase 2
# blocks in Thesis.R.
###############################################################
run_dynamic_ladder_simulation <- function(
    pot, wd, ladder_length, max_ladder_years, extend_by,
    bonds_fixed, valuation_date, convexity_buffer,
    equity_monthly_returns, horizon,
    cash_annual_rate, defend_cut, inflation_rate,
    start_age, max_age,
    res_redington_initial, C0_initial, bond_cf_initial,
    inflation_monthly_ratios = NULL
) {

  num_sims <- ncol(equity_monthly_returns)
  n_bonds  <- nrow(res_redington_initial$bond_metrics)
  bond_codes <- res_redington_initial$bond_metrics$bond_code
  cash_monthly_rate <- (1 + cash_annual_rate)^(1/12) - 1

  # Starting annual base payout, from the retiree's configured withdrawal-rate
  # parameter (wd) and starting wealth (pot) - both already threaded through
  # from Thesis.R's run parameters, not hardcoded here. Every path's
  # current_income starts at exactly this amount and is this path's own
  # "current base annual withdrawal" for CPI-indexation purposes thereafter
  # (see income_reset_month below).
  base_annual_withdrawal <- wd * pot

  # Realized cumulative CPI index per path, built from the actual bootstrapped
  # monthly inflation path (same [horizon x num_sims] convention as
  # equity_monthly_returns) rather than a flat compounding assumption -
  # cum_inflation[m, s] / cum_inflation[r, s] is the realized inflation growth
  # factor between month r and month m for path s, used below to index each
  # path's own monthly cash withdrawal to what CPI has actually done for it,
  # not a deterministic average. inflation_rate (the flat scalar) is kept for
  # what it's still legitimately used for elsewhere - decision_matrix()'s
  # forward-looking annual raise and compute_funded_ratio()'s forward
  # liability projection - since neither can discount against inflation that
  # hasn't happened yet. If no stochastic path is supplied, the monthly
  # payout falls back to compounding inflation_rate flat, so any caller not
  # yet passing one still works unchanged.
  cum_inflation <- if (!is.null(inflation_monthly_ratios)) {
    apply(inflation_monthly_ratios, 2, cumprod)
  } else {
    NULL
  }

  # ---- Per-path state -----------------------------------------------------
  phase                <- rep("ladder", num_sims)     # "ladder" or "arva"
  ladder_years_current <- rep(ladder_length, num_sims)
  current_income       <- rep(base_annual_withdrawal, num_sims)
  # Month at which current_income[s] was last (re)established - month 1 at
  # simulation start, then reset to the review month m whenever Extend &
  # Harvest / Defend changes current_income. Cumulative CPI for the monthly
  # payout (below) is measured from this month, so the annual review's own
  # extend/defend adjustment and the monthly inflation step-up compound
  # cleanly instead of double-counting a year of inflation.
  income_reset_month   <- rep(1L, num_sims)
  infeasible_flag       <- rep(FALSE, num_sims)
  bond_sale_amount      <- rep(NA_real_, num_sims)  # set once, at the moment each path's ladder ends
  duration_matched_flag <- rep(TRUE, num_sims)   # FALSE once a path has used the relaxed fallback
  ladder_end_month      <- rep(NA_integer_, num_sims)
  arva_year_index       <- rep(0L, num_sims)           # 0 until path enters ARVA

  # bond_holdings[sim, bond] = units held; identical starting position
  # for every path (the one-off initial optimization already run in
  # Thesis.R and passed in here)
  bond_holdings <- matrix(0, nrow = num_sims, ncol = n_bonds,
                          dimnames = list(NULL, bond_codes))
  bond_holdings[, names(res_redington_initial$bond_units)] <-
    matrix(res_redington_initial$bond_units, nrow = num_sims, ncol = n_bonds, byrow = TRUE)

  cash_account <- rep(C0_initial, num_sims)
  EPort        <- rep((pot - res_redington_initial$total_cost - C0_initial), num_sims)

  # Deposit schedule (coupons/redemption landing in the cash account) per
  # path, month-indexed relative to valuation_date - starts as the initial
  # ladder's schedule, replaced wholesale whenever a path rebalances.
  deposits_by_month <- matrix(0, nrow = horizon, ncol = num_sims)
  init_deposits <- bond_cf_initial$amount
  init_months   <- pmin(pmax(bond_cf_initial$month_index, 1), horizon)
  for (k in seq_along(init_deposits)) {
    deposits_by_month[init_months[k], ] <- deposits_by_month[init_months[k], ] + init_deposits[k]
  }

  # ---- History matrices (for plotting/diagnostics, same shape as before) --
  EPort_history           <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_history            <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_deposit_history    <- matrix(0, nrow = horizon,     ncol = num_sims)
  Cash_withdrawal_history <- matrix(0, nrow = horizon,     ncol = num_sims)
  ARVA_withdrawal_history <- matrix(NA_real_, nrow = 30, ncol = num_sims) # up to 30 possible ARVA years
  decision_log <- data.frame(sim = integer(0), year = integer(0), action = character(0),
                             funded_ratio = numeric(0), equity_return = numeric(0),
                             withdrawal = numeric(0), ladder_years = integer(0),
                             duration_matched = logical(0),
                             bonds_bought = character(0),
                             horizon_years = numeric(0))

  EPort_history[1, ] <- EPort
  Cash_history[1, ]  <- cash_account

  # ARVA-phase-only monthly draw cache: set fresh every 12 months from
  # EPort / arva_annuity_factor() (see the cash mechanics loop below) before
  # it is ever read, so its initial value here is never actually used. The
  # ladder phase no longer uses a cached monthly figure at all - its payout
  # is recomputed every month from current_income[s] and cumulative CPI
  # since income_reset_month[s] (see below), so it can step up smoothly
  # between annual reviews rather than staying flat for 12 months.
  current_monthly_withdrawal <- rep(NA_real_, num_sims)

  # ---- Main monthly loop ---------------------------------------------------
  for (m in 1:horizon) {

    # --- Annual ladder check: fires at the START of each ladder year from
    # year 2 onward (month 13, 25, 37, ...) so a full trailing 12-month
    # equity_return is always available. Only applies to paths still in
    # "ladder" phase.
    is_anniversary <- (m > 12) && ((m - 1) %% 12 == 0)
    ladder_idx <- which(phase == "ladder")

    # Every path checking this month shares the same valuation_date_now, so
    # its bond metrics (independent of any path's holdings/liability) are
    # computed exactly ONCE per month here, not once per path - this was a
    # real performance bug in the first draft (calculate_bond_metrics() walks
    # all 11 bonds' cash flow schedules, and was being redone per-path,
    # per-check, for identical output every time).
    bond_metrics_now <- NULL
    if (is_anniversary) {
      valuation_date_now <- valuation_date + months(m - 1)
      bond_metrics_now <- calculate_bond_metrics(bonds_fixed, valuation_date_now)
    }

    if (is_anniversary && length(ladder_idx) > 0) {
      for (s in ladder_idx) {

        bond_value_now <- mark_to_model_bond_value(bond_holdings[s, ], valuation_date_now, bond_metrics_now)
        remaining_years <- ladder_years_current[s] - (m - 1) / 12

        # 1. Exit check FIRST, unconditional, before remaining_years or
        # decision_matrix() are used for anything else: this path's CURRENT
        # target ladder length has fully elapsed in real time (remaining_years
        # <= 0). Checking this HERE, before remaining_years is used anywhere
        # else, matters: R's `1:remaining_years` for remaining_years = 0
        # evaluates to c(1, 0), NOT an empty vector, which would have
        # silently fed decision_matrix() a bogus two-cashflow liability
        # instead of ending the ladder.
        #
        # IMPORTANT: hitting max_ladder_years does NOT by itself end the
        # ladder here - it only stops the ladder from being extended any
        # further (enforced below, where new_ladder_years is capped via
        # min(..., max_ladder_years)). A path that reaches the cap still
        # plays out its already-immunized bond portfolio for real time,
        # all the way to that capped target date, and only liquidates once
        # remaining_years actually reaches zero - exactly like a path that
        # never got extended at all. Liquidating the instant the target
        # hits the cap (the old behaviour) was a bug: it cut potentially
        # many years off a path's ladder purely because the target number
        # matched max_ladder_years, regardless of how much real time was
        # actually left to run.
        if (remaining_years <= 1e-9) {
          EPort[s] <- EPort[s] + bond_value_now
          bond_sale_amount[s] <- bond_value_now
          bond_holdings[s, ] <- 0
          phase[s] <- "arva"
          ladder_end_month[s] <- m - 1
          decision_log <- rbind(decision_log, data.frame(
            sim = s, year = (m - 1) / 12,
            action = if (ladder_years_current[s] >= max_ladder_years) "Cap Reached - Liquidate to Equity" else "Ladder Matured - Liquidate to Equity",
            funded_ratio = NA, equity_return = NA,
            withdrawal = current_income[s], ladder_years = ladder_years_current[s],
            duration_matched = NA,
            bonds_bought = "LIQUIDATED",
            horizon_years = NA
          ))
          next
        }

        # 2. Skip paths already frozen due to an earlier infeasible rerun
        if (infeasible_flag[s]) next

        # 3. Funded ratio (full remaining lifetime, i.e. every future
        # expected expense from now through max_age - NOT just the years
        # left on the current ladder target) & trailing 12-month equity return
        age_now <- start_age + (m - 1) / 12
        remaining_lifetime_years <- max_age - age_now
        fr <- compute_funded_ratio(
          equity_value             = EPort[s],
          cash_value                = cash_account[s],
          bond_value                = bond_value_now,
          expected_annual_expense   = current_income[s],
          horizon_years             = remaining_lifetime_years,
          inflation_rate            = inflation_rate
        )
        trailing_ret <- prod(equity_monthly_returns[(m - 12):(m - 1), s]) - 1

        dec <- decision_matrix(
          funded_ratio    = fr$funded_ratio,
          equity_return   = trailing_ret,
          inflation_rate  = inflation_rate,
          current_income  = current_income[s],
          defend_cut      = defend_cut
        )

        decision_log <- rbind(decision_log, data.frame(
          sim = s, year = (m - 1) / 12, action = dec$action,
          funded_ratio = fr$funded_ratio, equity_return = trailing_ret,
          withdrawal = dec$withdrawal, ladder_years = ladder_years_current[s],
          duration_matched = NA,
          bonds_bought = NA_character_,
          horizon_years = remaining_lifetime_years
        ))

        if (dec$action %in% c("Wait & Hold", "Repair")) next  # nothing changes

        # Extend or Defend both change current_income AND require a rebalance
        new_ladder_years <- ladder_years_current[s]
        if (dec$extend_ladder) {
          new_ladder_years <- min(ladder_years_current[s] + extend_by, max_ladder_years)
        }
        new_remaining_years <- new_ladder_years - (m - 1) / 12

        # Size this rebalance's cash buffer against THIS path's own realized
        # future inflation (already known - inflation_monthly_ratios covers
        # the whole horizon up front, same as the equity return paths) rather
        # than the flat expected inflation_rate, so the buffer isn't caught
        # short by a stretch of realized inflation running well above the
        # flat assumption (observed in testing: ~25% annualized over one
        # path's final ladder years, versus the 5% flat rate). future_idx[1]
        # = m so the escalation factor is exactly 1.0 in the month this
        # withdrawal is (re)established, matching income_reset_month's own
        # convention below. Clamped to horizon and the last available factor
        # repeated beyond it, for a ladder sized to run past month `horizon`.
        inflation_factors_s <- NULL
        if (!is.null(cum_inflation)) {
          months_needed <- ceiling(new_remaining_years * 12)
          future_idx <- pmin(m + 0:(months_needed - 1), horizon)
          inflation_factors_s <- cum_inflation[future_idx, s] / cum_inflation[m, s]
        }

        rb <- rebalance_ladder(
          remaining_years      = new_remaining_years,
          annual_withdrawal    = dec$withdrawal,
          valuation_date_now   = valuation_date_now,
          current_bond_value   = bond_value_now,
          current_cash_value   = cash_account[s],
          convexity_buffer     = convexity_buffer,
          cash_annual_rate     = cash_annual_rate,
          bond_metrics_precomputed = bond_metrics_now,
          inflation_rate       = inflation_rate,
          inflation_factors    = inflation_factors_s
        )

        if (!rb$feasible) {
          infeasible_flag[s] <- TRUE   # freeze: keep existing holdings, stop trying to rebalance
          next
        }

        duration_matched_flag[s] <- rb$duration_matched
        decision_log$duration_matched[nrow(decision_log)] <- rb$duration_matched
        decision_log$bonds_bought[nrow(decision_log)] <- paste(
          sprintf("%s: %.2f", names(rb$bond_units), rb$bond_units),
          collapse = "; "
        )

        # Fund the harvest (or absorb the release) via the equity sleeve
        EPort[s] <- EPort[s] - rb$harvest_needed
        cash_account[s] <- rb$new_C0

        # Swap holdings to the new target
        bond_holdings[s, ] <- 0
        bond_holdings[s, names(rb$bond_units)] <- rb$bond_units

        # Replace this path's future deposit schedule from here on with the
        # new ladder's schedule
        deposits_by_month[m:horizon, s] <- 0
        new_deposits <- rb$bond_cf$amount
        new_months   <- pmin(pmax(rb$bond_cf$month_index + (m - 1), m), horizon)
        for (k in seq_along(new_deposits)) {
          deposits_by_month[new_months[k], s] <- deposits_by_month[new_months[k], s] + new_deposits[k]
        }

        ladder_years_current[s] <- new_ladder_years
        current_income[s] <- dec$withdrawal
        # Re-anchor the CPI clock: this review's Extend/Defend adjustment IS
        # this path's new base_annual_withdrawal going forward, so cumulative
        # CPI for the monthly payout (below) restarts counting from THIS
        # month rather than compounding on top of the year just applied.
        income_reset_month[s] <- m
      }
    }

    # (Natural ladder maturity and cap-reached liquidation are both handled
    # as the first-priority check inside the per-path loop above.)

    # --- Cash account mechanics (same for every path, ladder or ARVA,
    # just fed by whichever deposit/withdrawal schedule currently applies) --
    for (s in 1:num_sims) {
      if (phase[s] == "ladder") {
        # Monthly outflow indexation: cumulative CPI since this path's
        # current base_annual_withdrawal was last (re)established
        # (income_reset_month[s] - month 1 at simulation start, or the month
        # of its most recent Extend & Harvest / Defend review), applied to
        # that base every month so the cash deduction steps up smoothly
        # month-to-month instead of jumping only once a year at review time.
        # Uses this path's own REALIZED CPI path when the caller supplied
        # one (cum_inflation, from inflation_monthly_ratios) - each path can
        # therefore see a genuinely different cost-of-living trajectory, not
        # a single deterministic curve shared by all 1,000 paths - and falls
        # back to flat compounding at inflation_rate otherwise.
        cumulative_cpi_m <- if (!is.null(cum_inflation)) {
          cum_inflation[m, s] / cum_inflation[income_reset_month[s], s] - 1
        } else {
          (1 + inflation_rate)^((m - income_reset_month[s]) / 12) - 1
        }
        monthly_payment_m  <- (current_income[s] / 12) * (1 + cumulative_cpi_m)

        cash_account[s] <- cash_account[s] + deposits_by_month[m, s]
        cash_account[s] <- cash_account[s] - monthly_payment_m
        Cash_deposit_history[m, s]    <- deposits_by_month[m, s]
        Cash_withdrawal_history[m, s] <- monthly_payment_m
      } else {
        # ARVA: take the annual withdrawal at the first month of this path's
        # own ARVA phase and every 12 months thereafter (relative to its own
        # start, not a global calendar point)
        months_into_arva <- m - ladder_end_month[s] - 1
        if (months_into_arva %% 12 == 0) {
          arva_year_index[s] <- arva_year_index[s] + 1
          age_now <- start_age + ladder_end_month[s] / 12 + (arva_year_index[s] - 1)
          af <- arva_annuity_factor(age_now, max_age = max_age)
          w <- min(EPort[s] / af, EPort[s])
          ARVA_withdrawal_history[arva_year_index[s], s] <- w
          EPort[s] <- max(EPort[s] - w, 0)
          cash_account[s] <- cash_account[s] + w
          current_monthly_withdrawal[s] <- w / 12
        }
        cash_account[s] <- cash_account[s] - current_monthly_withdrawal[s]
        Cash_deposit_history[m, s]    <- if (months_into_arva %% 12 == 0) ARVA_withdrawal_history[arva_year_index[s], s] else 0
        Cash_withdrawal_history[m, s] <- current_monthly_withdrawal[s]
      }
      cash_account[s] <- cash_account[s] * (1 + cash_monthly_rate)
    }
    Cash_history[m + 1, ] <- cash_account

    # --- Equity compounds for everyone, every month, regardless of phase ---
    EPort <- EPort * equity_monthly_returns[m, ]
    EPort_history[m + 1, ] <- EPort
  }

  list(
    EPort_history           = EPort_history,
    Cash_history            = Cash_history,
    Cash_deposit_history    = Cash_deposit_history,
    Cash_withdrawal_history = Cash_withdrawal_history,
    ARVA_withdrawal_history = ARVA_withdrawal_history,
    ladder_end_month        = ladder_end_month,
    ladder_years_final      = ladder_years_current,
    infeasible_flag         = infeasible_flag,
    bond_sale_amount        = bond_sale_amount,
    duration_matched_flag   = duration_matched_flag,
    decision_log            = decision_log,
    EPort                   = EPort
  )
}
