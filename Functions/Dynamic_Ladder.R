###############################################################
# Dynamic Bond Ladder - simulation engine
###############################################################
# The bond ladder is the only thing in this project that lives in REAL terms.
# EPort, the cash account, ARVA, the funded ratio and every rand actually paid
# to the retiree are NOMINAL. The two worlds meet in exactly one function -
# real_to_nominal_factor() (Functions/ILB_Indexation.R) - and every crossing in
# this file goes through it.
#
# ------------------------------------------------------------------------------
# WHAT CHANGED IN THE INVARIANT REWRITE, AND WHY
# ------------------------------------------------------------------------------
# Five defects in the previous version were all one mistake: a REAL rand amount
# used where a NOMINAL one belonged. In this file specifically:
#
#   * the ladder mark (reprice_ladder) was over-stated by each bond's t0 index
#     ratio, ~1.80x on the base-case ladder AT T0 - inflating every funded ratio
#     and the lump sum credited to equity at liquidation;
#   * extend_ilb_ladder()'s bond_cost and cash_top_up came back in T0-REAL rands
#     and were charged straight to nominal EPort and nominal cash, making every
#     extension progressively cheaper than it really was;
#   * compute_funded_ratio() was handed a REAL income as though it were the
#     current nominal spend;
#   * `current_income` silently CHANGED DENOMINATION mid-run - t0-real until the
#     first decision fired, then reset-date-nominal after it - because
#     `income_reset_month` re-based the CPI indexation on every reset. The 5%
#     "inflation raise" only cancelled that re-basing when a reset happened
#     every 12 months AND realized inflation was exactly the assumed rate. Off
#     either condition the real income drifted with no decision having been
#     taken: at 10% realized inflation it eroded from R600,000 to R453,869 over
#     seven years, and a single Extend after three Hold years cut it from
#     R600,000 to R518,303 - a 14% real cut delivered by the decision labelled
#     "raise income".
#
# The income mechanic is now stated once and does not move:
#
#     real_income[s]      a T0-REAL annual level. Set at t0, changed ONLY by a
#                         Defend cut. Extend & Harvest extends the LADDER; it
#                         does not change the standard of living, because CPI
#                         indexation already preserves purchasing power.
#     payment(m, s)       = real_income[s]/12 * factor(m, s)
#
# `income_reset_month` is gone. Indexation always runs from t0, so the real
# income is flat under ANY realized inflation path rather than only under one.
#
# Cash-account ordering was also aligned to the selector's. The engine used to
# credit interest AFTER the month's flows, while ilb_ladder_real_schedule()
# credits it on the OPENING balance, before them - so the engine earned an extra
# month of interest on every coupon and every withdrawal. Both now do:
#
#     opening(m) = closing(m-1) * (1 + i)      (no interest in month 1)
#     closing(m) = opening(m) + deposits(m) - withdrawal(m)
#
# which is what makes boundary_check_golden_path() (Functions/Invariants.R) able
# to hold the engine to the selector's schedule to the cent.
#
# rebalance_ladder() and mark_to_model_bond_value() (the old Redington-era
# helpers) were REMOVED, not adapted: extend_ilb_ladder() and reprice_ladder()
# replace them and neither is a thin rename.
###############################################################

# ------------------------------------------------------------------------------
# Funded ratio: total assets (equity + cash + current bond ladder value) over the
# present value of ALL future expected retirement expenses - every withdrawal
# from now until max_age, not just the years remaining on the CURRENT ladder.
# funded_ratio >= 1 means total assets can support every future expense, not
# merely "can fund the ladder to its own end".
#
# UNITS: every input is NOMINAL, as at this path's current month.
# expected_annual_expense must be the income the retiree is being paid RIGHT NOW
# in today's rands - i.e. real_income[s] * factor(m, s), not real_income[s].
# Passing the raw real level understates the liability by cumulative inflation
# and biases the whole model toward Extend & Harvest; the audit check
# "funded ratio matches income paid" in the loop below is the regression guard.
#
# Each future year's expense is grown from expected_annual_expense at the flat
# inflation_rate (a forward-looking EXPECTATION - you cannot discount against
# inflation that has not happened yet) and discounted at the NOMINAL curve
# (path-dependent, so supplied by the caller per path, not cached per month).
# ------------------------------------------------------------------------------
compute_funded_ratio <- function(equity_value, cash_value, bond_value,
                                 expected_annual_expense, horizon_years,
                                 inflation_rate = 0,
                                 nominal_curve_vec, nominal_tenors_months,
                                 bequest_nominal_now = 0) {

  t_vec <- 1:horizon_years
  y_vec <- real_curve_yield(t_vec, nominal_curve_vec, nominal_tenors_months) / 100
  inflated_expense_t <- expected_annual_expense * (1 + inflation_rate)^t_vec
  pv_income <- sum(inflated_expense_t * (1 + y_vec)^(-t_vec))

  # THE BEQUEST IS PART OF THE LIABILITY.
  # The retiree stated two goals - an income AND something left at the end - so
  # "funded" has to mean able to meet both. Pricing only the income made the
  # ratio flatter than the retiree's own objective and is a large part of why
  # Extend & Harvest fired on most reviews even on paths that ended far below
  # target. Projected forward at the flat inflation_rate and discounted on the
  # nominal curve - the same convention as the income leg above, deliberately,
  # so one calculation does not use two conventions.
  y_T <- real_curve_yield(horizon_years, nominal_curve_vec, nominal_tenors_months) / 100
  pv_bequest <- bequest_nominal_now * (1 + inflation_rate)^horizon_years *
                (1 + y_T)^(-horizon_years)

  cost_to_fully_fund <- pv_income + pv_bequest

  total_assets <- equity_value + cash_value + bond_value

  list(
    funded_ratio       = total_assets / cost_to_fully_fund,
    cost_to_fully_fund = cost_to_fully_fund,
    pv_income          = pv_income,
    pv_bequest         = pv_bequest,
    total_assets       = total_assets
  )
}

###############################################################
# Main simulation: dynamic ladder phase + ARVA phase, all paths,
# all months, in one loop.
###############################################################
# MONTH CONVENTION (see Functions/ILB_Indexation.R for the full statement):
#   m  1..horizon   simulation month, the SPAN [t0 + (m-1)mo, t0 + m mo)
#   k  = m - 1      elapsed months, the INSTANT at the START of month m
# Everything that happens "in month m" - the annuity-due withdrawal, the annual
# review, an extension purchase - is valued at k. That is why reprice_ladder()
# and extend_ilb_ladder() are called with month = k and not month = m.
#
# decisions_enabled = FALSE runs the ladder with the decision matrix switched
# off: no extensions, no cuts, income level for the whole horizon. That is the
# static comparator arm, and it is what boundary_check_golden_path() drives.
#
# EXTENSION CASH: the engine funds a SHORTFALL, not a requirement.
# extend_ilb_ladder() returns the opening balance its sub-ladder needs on
# solve_start_date (cash_required_real) and the month that date falls in. The
# engine projects its OWN pooled cash account forward to that month - through the
# deposit schedule it already holds, including the coupons of the bonds being
# bought right now - and harvests from equity only the gap, if any.
#
# The old code charged the full requirement every year. Because the anchor often
# stays put for several consecutive extensions, that funded the same bridge three
# or four times over, harvesting equity each time and parking the proceeds in
# cash at a negative real rate. It is also why cash_buffer_months is NOT passed
# down here: the initial ladder is bought with a stated margin, and each
# extension is then sized against the margin the account is actually projected to
# still be carrying, rather than stacking a fresh one on top.
#
# THE CASH ACCOUNT IS STILL ALLOWED TO GO NEGATIVE. That is a deliberate
# modelling choice, not an oversight: nothing harvests equity to cover a short
# cash balance. It is counted per path (cash_negative_months) and reported. What
# has changed is that it is no longer FREE - terminal wealth is now equity PLUS
# residual cash, so a path that ends in overdraft is scored on the overdraft.
#
# audit: an accumulator from audit_new() (Functions/Invariants.R), or NULL to
# skip all checking.
###############################################################
run_dynamic_ladder_simulation <- function(
    pot, wd, ladder_length, max_ladder_years, extend_by,
    ilb_bonds, valuation_date,
    equity_monthly_returns, horizon,
    cash_annual_rate, defend_cut, inflation_rate,
    start_age, max_age,
    res_ilb_initial,
    yc, real_yc, reference_cpi,
    cpi_t0 = ILB_REFERENCE_CPI,
    bequest_target_real = 0,
    decisions_enabled = TRUE,
    audit = NULL
) {

  num_sims   <- ncol(equity_monthly_returns)
  n_bonds    <- nrow(ilb_bonds)
  bond_codes <- ilb_bonds$bond_code
  cash_monthly_rate <- (1 + cash_annual_rate)^(1/12) - 1

  # The real -> nominal bridge for every (month, path), built once. Row m is the
  # factor for elapsed month m-1, i.e. the factor that applies to simulation
  # month m. This single object replaces the old `cum_inflation` matrix, which
  # was a second copy of the same numbers derived from the same bootstrap by a
  # different route - two objects that had to agree, now one that cannot
  # disagree. assert_cpi_anchor() (called from Thesis.R) is what makes them
  # provably the same series.
  nfac <- nominal_factor_matrix(reference_cpi, cpi_t0)
  if (nrow(nfac) < horizon + 1) {
    stop(sprintf("reference_cpi has %d months but the horizon is %d - the CPI path must cover the run",
                 nrow(reference_cpi), horizon))
  }
  if (ncol(nfac) < num_sims) {
    stop(sprintf("reference_cpi has %d paths but equity_monthly_returns has %d - these must match",
                 ncol(reference_cpi), num_sims))
  }

  # Starting REAL annual withdrawal, in t0 rands. LEVEL by construction, which
  # is the whole point of a real ladder.
  base_annual_withdrawal <- wd * pot

  # ---- Per-path state -----------------------------------------------------
  phase                <- rep("ladder", num_sims)     # "ladder" or "arva"
  ladder_years_current <- rep(ladder_length, num_sims)
  # T0-REAL annual income. Only a Defend cut ever changes this. There is no
  # reset month: indexation always runs from t0.
  real_income          <- rep(base_annual_withdrawal, num_sims)
  bond_sale_amount     <- rep(NA_real_, num_sims)
  ladder_end_month     <- rep(NA_integer_, num_sims)
  arva_year_index      <- rep(0L, num_sims)
  # TRUE once this path's anchor (latest-redeeming HELD bond) has matured with
  # no replacement bought. Once set, no further purchase is attempted: the
  # ladder is considered set and the existing cash runs down to the committed
  # target. NOTE: on this universe, with max_ladder_years = 15, this is close to
  # structural rather than rare - a path that extends steadily hits the cap
  # around year 10 with I2038 as its anchor (redeeming in month 138) and a
  # ladder running to month 180, and there is nothing between I2038 and I2043 to
  # buy, so the anchor matures ~3.5 years before the ladder ends.
  ladder_locked        <- rep(FALSE, num_sims)

  # units held, in ilb_fixed's own units (R100 of T0-indexed par). Sized to the
  # FULL universe, since an extension can buy a bond that was not eligible at t0.
  bond_holdings <- matrix(0, nrow = num_sims, ncol = n_bonds,
                          dimnames = list(NULL, bond_codes))
  bond_holdings[, names(res_ilb_initial$bond_units)] <-
    matrix(res_ilb_initial$bond_units, nrow = num_sims, ncol = n_bonds, byrow = TRUE)

  cash_account <- rep(res_ilb_initial$cash_at_start, num_sims)
  EPort        <- rep(pot - res_ilb_initial$total_outlay, num_sims)

  # ---- Deposit schedules ---------------------------------------------------
  # Two schedules are carried, deliberately:
  #   real_deposits    the ladder's CONTRACTUAL flows in t0 rands. Path-
  #                    independent by construction (a real flow is a real flow),
  #                    so the initial one is computed once.
  #   deposits_by_month the nominal rands that actually land in the cash
  #                    account, built per path by nominal_deposits_for_holdings().
  # The nominal one is NOT derived from the real one - it is built independently
  # from the same contractual cash flows - so the audit check at the end of the
  # run ("deposit conversion") genuinely tests nominal_bond_deposits()'s
  # internals rather than restating its own arithmetic.
  real_deposits     <- matrix(0, nrow = horizon, ncol = num_sims)
  deposits_by_month <- matrix(0, nrow = horizon, ncol = num_sims)

  real_dep_initial <- real_deposits_for_holdings(
    res_ilb_initial$bond_units, ilb_bonds,
    purchase_date = valuation_date, global_valuation_date = valuation_date,
    horizon = horizon
  )
  for (s in 1:num_sims) {
    real_deposits[, s]     <- real_dep_initial
    deposits_by_month[, s] <- nominal_deposits_for_holdings(
      res_ilb_initial$bond_units, ilb_bonds,
      purchase_date = valuation_date, global_valuation_date = valuation_date,
      reference_cpi_col = reference_cpi[, s], horizon = horizon, cpi_t0 = cpi_t0
    )
  }

  # ---- History ------------------------------------------------------------
  EPort_history           <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_history            <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_deposit_history    <- matrix(0, nrow = horizon,     ncol = num_sims)
  Cash_withdrawal_history <- matrix(0, nrow = horizon,     ncol = num_sims)
  Cash_interest_history   <- matrix(0, nrow = horizon,     ncol = num_sims)
  Income_real_history     <- matrix(0, nrow = horizon,     ncol = num_sims)
  ARVA_withdrawal_history <- matrix(NA_real_, nrow = 30, ncol = num_sims)

  # decision_log is collected as a list and bound ONCE. The old rbind-per-row
  # was quadratic - on 1,000 paths x 30 reviews that is 30,000 reallocations of
  # a growing data.frame.
  log_rows <- vector("list", 0)
  log_i    <- 0L
  add_log  <- function(row) { log_i <<- log_i + 1L; log_rows[[log_i]] <<- row }

  EPort_history[1, ] <- EPort
  Cash_history[1, ]  <- cash_account

  current_monthly_withdrawal <- rep(NA_real_, num_sims)

  # ---- Ledger accumulators, for the terminal reconciliation ----------------
  acc_equity_gain      <- rep(0, num_sims)
  acc_cash_interest    <- rep(0, num_sims)
  acc_deposits         <- rep(0, num_sims)
  acc_income_paid      <- rep(0, num_sims)
  acc_bond_cost        <- rep(0, num_sims)   # NOMINAL rands spent on bonds after t0
  acc_bond_liquidation <- rep(0, num_sims)
  n_unaffordable       <- 0L
  cash_negative_months <- rep(0L, num_sims)
  acc_cash_topup       <- rep(0, num_sims)   # nominal, harvested for cash bridges
  n_extensions         <- rep(0L, num_sims)
  acc_arva_transfer    <- rep(0, num_sims)   # equity -> cash, ARVA phase only
  arva_floor_years     <- rep(0L, num_sims)  # ARVA years where the income floor bound

  # Carries the nominal annual expense the funded ratio was computed against, so
  # it can be checked against the payment the cash mechanics ACTUALLY make later
  # in the same month. NA means "not reviewed this month, or reviewed and then
  # Defended" - a Defend changes the income between the two points, so those
  # paths are excluded rather than falsely flagged.
  fr_expense <- rep(NA_real_, num_sims)

  # ---- Main monthly loop ---------------------------------------------------
  for (m in 1:horizon) {

    k           <- m - 1L                      # elapsed months: the start of month m
    curve_month <- max(k, 1L)                  # yc/real_yc row 1 is one month after t0

    fr_expense[]   <- NA_real_
    is_anniversary <- (m > 12) && ((m - 1) %% 12 == 0)
    ladder_idx     <- which(phase == "ladder")

    if (is_anniversary && length(ladder_idx) > 0) {
      valuation_date_now <- valuation_date %m+% months(k)

      for (s in ladder_idx) {

        remaining_years <- ladder_years_current[s] - k / 12

        # 1. Exit check FIRST, unconditional, before remaining_years is used for
        # anything else (in R, 1:0 is c(1, 0), not an empty vector). Hitting
        # max_ladder_years does NOT end the ladder - it only caps further
        # extension; the path still runs its portfolio to the capped target.
        if (remaining_years <= 1e-9) {
          # Per-path repricing: the real curve is stochastic PER PATH, so this
          # cannot be hoisted out of the loop - doing so would silently apply one
          # path's curve to every path. It is also why the run is slow.
          marked         <- reprice_ladder(bond_holdings[s, ], ilb_bonds, valuation_date,
                                           k, s, real_yc, reference_cpi, cpi_t0)
          bond_value_now <- sum(marked$rand_value)

          EPort[s] <- EPort[s] + bond_value_now
          acc_bond_liquidation[s] <- acc_bond_liquidation[s] + bond_value_now
          bond_sale_amount[s] <- bond_value_now
          bond_holdings[s, ]  <- 0
          phase[s]            <- "arva"
          ladder_end_month[s] <- m - 1
          add_log(data.frame(
            sim = s, year = k / 12,
            action = if (ladder_years_current[s] >= max_ladder_years) "Cap Reached - Liquidate to Equity" else "Ladder Matured - Liquidate to Equity",
            funded_ratio = NA, equity_return = NA,
            withdrawal = real_income[s], ladder_years = ladder_years_current[s],
            bonds_bought = "LIQUIDATED", horizon_years = NA,
            stringsAsFactors = FALSE
          ))
          next
        }

        if (!decisions_enabled) next

        held_now       <- bond_holdings[s, ]
        marked         <- reprice_ladder(held_now, ilb_bonds, valuation_date,
                                         k, s, real_yc, reference_cpi, cpi_t0)
        bond_value_now <- sum(marked$rand_value)

        # 2. Funded ratio & trailing 12-month equity return - both NOMINAL,
        # on this path's own nominal curve (never the real one).
        age_now                  <- start_age + k / 12
        remaining_lifetime_years <- max_age - age_now
        nominal_curve_vec        <- yc$get_curve(month = curve_month, sim = s)

        # The current nominal spend. THIS is the conversion the old code missed:
        # it passed real_income[s] straight through, understating the liability
        # by cumulative inflation and biasing every review toward extending.
        # Stashed for the cross-check against the payment actually made below.
        nominal_income_now <- real_income[s] * nfac[m, s]
        fr_expense[s]      <- nominal_income_now

        fr <- compute_funded_ratio(
          equity_value          = EPort[s],
          cash_value            = cash_account[s],
          bond_value            = bond_value_now,
          expected_annual_expense = nominal_income_now,
          horizon_years         = remaining_lifetime_years,
          inflation_rate        = inflation_rate,
          nominal_curve_vec     = nominal_curve_vec,
          nominal_tenors_months = yc$tenors_months,
          bequest_nominal_now   = bequest_target_real * nfac[m, s]
        )
        trailing_ret <- prod(equity_monthly_returns[(m - 12):(m - 1), s]) - 1

        dec <- decision_matrix(
          funded_ratio   = fr$funded_ratio,
          equity_return  = trailing_ret,
          inflation_rate = inflation_rate,
          current_income = real_income[s],
          defend_cut     = defend_cut
        )

        this_log <- data.frame(
          sim = s, year = k / 12, action = dec$action,
          funded_ratio = fr$funded_ratio, equity_return = trailing_ret,
          withdrawal = real_income[s], ladder_years = ladder_years_current[s],
          bonds_bought = NA_character_, horizon_years = remaining_lifetime_years,
          stringsAsFactors = FALSE
        )

        if (dec$action %in% c("Wait & Hold", "Repair")) { add_log(this_log); next }

        if (dec$action == "Defend") {
          # A REAL cut to the standard of living, permanent. No holdings change:
          # existing bonds now over-fund their already-committed months
          # (accepted limitation of the additive-only design - it can extend
          # forward but cannot shrink or patch a block already bought).
          real_income[s]      <- dec$withdrawal
          fr_expense[s]       <- NA_real_   # income changed after the ratio was taken
          this_log$withdrawal <- real_income[s]
          add_log(this_log)
          next
        }

        # --- Extend & Harvest ---------------------------------------------
        # Income is NOT touched here. Extending buys more ladder; purchasing
        # power is already preserved by indexing the payment from t0.

        # Lazily detect locking: has the anchor already matured as of this
        # review, with nothing bought to replace it?
        if (!ladder_locked[s]) {
          held_codes  <- names(held_now)[held_now > 0]
          anchor_date <- max(ilb_bonds$redemption_date[match(held_codes, ilb_bonds$bond_code)])
          if (anchor_date <= valuation_date_now) ladder_locked[s] <- TRUE
        }

        new_ladder_years <- min(ladder_years_current[s] + extend_by, max_ladder_years)
        capped           <- new_ladder_years <= ladder_years_current[s]

        if (ladder_locked[s] || capped) {
          this_log$bonds_bought <- if (ladder_locked[s]) {
            "(none - ladder locked, anchor already matured)"
          } else {
            "(none - capped, no room to extend further)"
          }
          add_log(this_log)
          next
        }

        ext <- extend_ilb_ladder(
          bond_units_held        = held_now,
          target_years           = new_ladder_years,
          valuation_date         = valuation_date,
          month                  = k,
          sim                    = s,
          annual_real_withdrawal = real_income[s],   # genuinely t0-real, always
          bonds                  = ilb_bonds,
          real_yc                = real_yc,
          reference_cpi          = reference_cpi,
          nominal_interest_rate  = cash_annual_rate,
          inflation_rate         = inflation_rate,
          cpi_t0                 = cpi_t0,
          on_infeasible          = "flag"
        )

        # The bonds bought today start paying coupons immediately, including
        # into the stretch between now and the sub-ladder's own start, so their
        # schedule has to exist before the account can be projected.
        inc_dep_nom <- nominal_deposits_for_holdings(
          ext$incremental_units, ilb_bonds,
          purchase_date = valuation_date_now, global_valuation_date = valuation_date,
          reference_cpi_col = reference_cpi[, s], horizon = horizon, cpi_t0 = cpi_t0
        )
        inc_dep_real <- real_deposits_for_holdings(
          ext$incremental_units, ilb_bonds,
          purchase_date = valuation_date_now, global_valuation_date = valuation_date,
          horizon = horizon
        )

        # --- how much cash does this extension ACTUALLY need? ----------------
        # Roll the pooled account forward from today to the month the sub-solve
        # starts in, through the mechanics it will actually experience:
        #
        #   opening(m*) = cash * g^(n+1) + SUM_j net_(m+j) * g^(n-j),   g = 1 + i
        #
        # in closed form rather than a loop, over months m .. m*-1. The
        # withdrawal leg assumes today's real income holds - a forward
        # assumption, and the right one: a later Defend cut only reduces it.
        #
        # Then fund the GAP between that and the requirement. In most years the
        # account is already carrying enough and the gap is zero, which is the
        # whole point: an anchor that has not moved does not need its bridge
        # bought a second time.
        m_star      <- ext$solve_start_month
        cash_top_up <- 0
        if (is.finite(m_star) && m_star <= horizon) {
          g    <- 1 + cash_monthly_rate
          n    <- max(0L, as.integer(m_star) - m)
          proj <- cash_account[s] * g^(n + 1)
          if (n > 0L) {
            idx  <- m:(m_star - 1L)
            net  <- (deposits_by_month[idx, s] + inc_dep_nom[idx]) -
                    (real_income[s] / 12) * nfac[idx, s]
            proj <- proj + sum(net * g^(n - seq_along(idx) + 1L))
          }
          needed      <- ext$cash_required_real * nfac[m_star, s]
          cash_top_up <- max(0, needed - proj)
        }
        harvest_needed <- ext$bond_cost + cash_top_up

        # Affordability. The funded-ratio test normally makes this impossible,
        # but nothing in the mechanics enforced it and equity was allowed to go
        # negative to pay for a ladder the retiree could not afford. Skip the
        # purchase rather than manufacture money.
        if (harvest_needed > EPort[s]) {
          n_unaffordable <- n_unaffordable + 1L
          this_log$bonds_bought <- sprintf("(none - unaffordable: needs R%.0f, equity R%.0f)",
                                           harvest_needed, EPort[s])
          add_log(this_log)
          next
        }

        assets_before <- EPort[s] + cash_account[s] + bond_value_now

        bought <- ext$incremental_units[ext$incremental_units > 0]
        this_log$bonds_bought <- if (length(bought)) {
          paste(sprintf("%s: %.2f", names(bought), bought), collapse = "; ")
        } else {
          "(none - top-up only reflected in existing holding)"
        }

        # Fund the purchase (bonds + the sub-ladder's own cash bridge) from
        # equity. Both legs are NOMINAL - converted at the boundary inside
        # extend_ilb_ladder(), which is the fix for the second defect.
        EPort[s]          <- EPort[s] - harvest_needed
        cash_account[s]   <- cash_account[s] + cash_top_up
        acc_bond_cost[s]  <- acc_bond_cost[s] + ext$bond_cost
        acc_cash_topup[s] <- acc_cash_topup[s] + cash_top_up
        n_extensions[s]   <- n_extensions[s] + 1L

        bond_holdings[s, ] <- bond_holdings[s, ] + ext$incremental_units

        # A purchase moves money between sleeves; it cannot create or destroy
        # any. If bond_cost were still charged in REAL rands while the bonds are
        # marked in NOMINAL ones, this is the identity that breaks. It costs a
        # second repricing of the whole holding, so it only runs when the audit
        # is switched on.
        if (!is.null(audit) && audit$enabled) {
          marked_after <- reprice_ladder(bond_holdings[s, ], ilb_bonds, valuation_date,
                                         k, s, real_yc, reference_cpi, cpi_t0)
          audit_check(audit, "extension conserves wealth",
                      EPort[s] + cash_account[s] + sum(marked_after$rand_value),
                      assets_before, sim = s, month = m,
                      note = "total assets immediately before an extension must equal total assets immediately after")
        }

        # ADD (never replace) this purchase's own future cash flows to both
        # schedules - computed above, before the projection that needed them.
        deposits_by_month[, s] <- deposits_by_month[, s] + inc_dep_nom
        real_deposits[, s]     <- real_deposits[, s]     + inc_dep_real

        ladder_years_current[s] <- new_ladder_years
        add_log(this_log)
      }
    }

    # --- Cash account mechanics ------------------------------------------
    # Ordering matches ilb_ladder_real_schedule() exactly:
    #   opening(m) = closing(m-1) * (1 + i)   [no interest in month 1]
    #   closing(m) = opening(m) + deposits - withdrawal
    # The engine used to credit interest on the CLOSING balance instead, earning
    # an extra month on every flow and putting it permanently out of step with
    # the selector's own schedule.
    cash_open <- cash_account
    interest  <- if (m > 1) cash_open * cash_monthly_rate else rep(0, num_sims)
    cash_open <- cash_open + interest
    acc_cash_interest <- acc_cash_interest + interest

    deposit_m    <- numeric(num_sims)
    withdrawal_m <- numeric(num_sims)

    for (s in 1:num_sims) {
      if (phase[s] == "ladder") {
        # Level real income, indexed from t0. One expression, one convention,
        # no reset month.
        withdrawal_m[s] <- (real_income[s] / 12) * nfac[m, s]
        deposit_m[s]    <- deposits_by_month[m, s]
      } else {
        # ARVA: take the annual withdrawal on each 12-month anniversary of THIS
        # path's own ladder end, then pay it out flat over the following year.
        # The annuity factor is built off the NOMINAL curve because EPort is a
        # nominal figure - see ARVA.R's own note.
        months_into_arva <- m - ladder_end_month[s] - 1
        if (months_into_arva %% 12 == 0) {
          arva_year_index[s] <- arva_year_index[s] + 1
          age_now <- start_age + ladder_end_month[s] / 12 + (arva_year_index[s] - 1)
          nominal_curve_vec <- yc$get_curve(month = curve_month, sim = s)
          af <- arva_annuity_factor(age_now, nominal_curve_vec, yc$tenors_months, max_age = max_age)
          # --- ARVA AMORTISES DOWN TO THE BEQUEST, NOT TO ZERO --------------
          # A plain ARVA spends the portfolio to nothing by max_age. This
          # retiree wants bequest_target_real (in t0 rands) left at the end, so
          # the annuity is struck on the portfolio NET OF the present value of
          # that target - the classic "annuity with a reserve" form. af itself
          # is untouched: still mortality-weighted, still on the nominal curve,
          # still running to max_age. Only the amount being amortised changes.
          #
          # The reserve is projected forward at the flat inflation_rate and
          # discounted on the nominal curve, matching compute_funded_ratio()
          # exactly, so the ladder phase and the ARVA phase now price the same
          # goal the same way.
          #
          # NOT survival-weighted, deliberately, and af is. That is a real
          # inconsistency and it is the honest one: af prices a life annuity,
          # but this model has no stochastic death - every path runs all 360
          # months - and a retiree wants the full bequest left whenever they
          # die, not the bequest times the probability of reaching max_age.
          #
          # --- ARVA IS SIZED ON THE WHOLE INVESTMENT PORTFOLIO --------------
          # The entitlement is EPort/af no longer: it is (equity + cash)/af.
          # The cash account is part of the retiree's wealth, not a conduit
          # attached to it, and pricing the annuity off equity alone both
          # understates what they can afford and ignores a balance that is
          # already sitting there.
          #
          # cash_open[s] is the right cash figure: the closing balance of month
          # m-1 with this month's interest already credited, i.e. the money
          # actually available at the review. cash_account[s] has not been
          # updated for month m yet at this point in the loop.
          portfolio <- EPort[s] + cash_open[s]

          years_left <- max_age - age_now
          pv_bequest <- if (bequest_target_real > 0 && years_left > 0) {
            y_T <- real_curve_yield(years_left, nominal_curve_vec, yc$tenors_months) / 100
            bequest_target_real * nfac[m, s] * (1 + inflation_rate)^years_left *
              (1 + y_T)^(-years_left)
          } else {
            bequest_target_real * nfac[m, s]
          }

          w_target <- max(0, (portfolio - pv_bequest) / af)

          # THE INCOME FLOOR. As years_left shrinks the reserve approaches the
          # full target, so a portfolio only just above it would otherwise pay
          # almost nothing for the last few years - a retiree starving to
          # protect their own estate. The floor is the real income the ladder
          # was securing (cut by any Defend, indexed to today): the retiree
          # never draws less than the level the whole strategy was built
          # around, and the bequest is sacrificed instead. On bad paths that
          # genuinely depletes the portfolio, which is the honest outcome.
          w_floor <- real_income[s] * nfac[m, s]
          w <- max(w_target, min(w_floor, max(0, portfolio)))
          w <- min(w, max(0, portfolio))
          if (w > w_target + 1e-9) arva_floor_years[s] <- arva_floor_years[s] + 1L

          ARVA_withdrawal_history[arva_year_index[s], s] <- w

          # ...AND IT SPENDS THE CASH IT ALREADY HAS FIRST.
          # Only the part of the year the existing balance cannot cover is
          # drawn from equity, so equity is left invested wherever the bank
          # account can carry the year on its own. When cash_open >= w the
          # transfer is zero and the whole year is funded from the balance
          # already held - which is what progressively drains the ladder-phase
          # residual instead of letting it ride to month 360.
          #
          # min(..., EPort[s]) keeps the transfer from manufacturing equity that
          # is not there; max(0, ...) stops a negative equity balance being
          # "sold". A shortfall neither sleeve can meet simply drives the cash
          # account negative, which is the accepted behaviour everywhere else.
          transfer <- max(0, min(w - cash_open[s], EPort[s]))
          EPort[s] <- EPort[s] - transfer
          acc_arva_transfer[s] <- acc_arva_transfer[s] + transfer
          deposit_m[s] <- transfer
          current_monthly_withdrawal[s] <- w / 12
        }
        # Interest accrues in the account and is NOT paid out separately. It
        # does not vanish into the bequest either: because the entitlement above
        # is struck on equity PLUS cash, every rand of interest earned raises
        # next year's w and is spent through that. (An earlier revision added
        # the interest to each month's payment directly; sizing ARVA on the full
        # portfolio subsumes it, so that was reverted.)
        withdrawal_m[s] <- current_monthly_withdrawal[s]
      }
    }

    cash_account <- cash_open + deposit_m - withdrawal_m

    audit_check(audit, "cash identity",
                cash_account, cash_open + deposit_m - withdrawal_m,
                sim = 1:num_sims, month = m,
                note = "opening x (1+i) + deposits - withdrawals == closing")

    # The funded ratio's liability must be the money the retiree is actually
    # being paid. The two are computed in different places from different
    # variables - the ratio from real_income x factor at review time, the payment
    # from the cash mechanics - so this genuinely tests that they agree, which is
    # the defect where a t0-REAL income was discounted as though it were the
    # current nominal spend.
    fr_idx <- which(!is.na(fr_expense))
    if (length(fr_idx)) {
      audit_check(audit, "funded ratio matches income paid",
                  fr_expense[fr_idx], withdrawal_m[fr_idx] * 12,
                  sim = fr_idx, month = m,
                  note = "the INCOME LEG of the funded ratio must equal 12x the payment actually made that month (the bequest leg is priced separately)")
    }
    audit_assert(audit, "cash balance is finite", is.finite(cash_account),
                 sim = 1:num_sims, month = m)

    cash_negative_months <- cash_negative_months + (cash_account < 0)

    # Only ladder-phase deposits are bond coupons; an ARVA transfer is equity
    # moving to cash, not new money, so it is excluded from the ledger term.
    acc_deposits    <- acc_deposits + ifelse(phase == "ladder", deposit_m, 0)
    acc_income_paid <- acc_income_paid + withdrawal_m

    Cash_deposit_history[m, ]    <- deposit_m
    Cash_withdrawal_history[m, ] <- withdrawal_m
    Cash_interest_history[m, ]   <- interest
    Income_real_history[m, ]     <- withdrawal_m / nfac[m, ]
    Cash_history[m + 1, ]        <- cash_account

    # --- Equity compounds for everyone, every month, regardless of phase ---
    gain            <- EPort * (equity_monthly_returns[m, ] - 1)
    acc_equity_gain <- acc_equity_gain + gain
    EPort           <- EPort + gain
    EPort_history[m + 1, ] <- EPort

    audit_assert(audit, "equity balance is finite", is.finite(EPort),
                 sim = 1:num_sims, month = m)
    audit_assert(audit, "real income never rises",
                 real_income <= base_annual_withdrawal + 1e-9,
                 sim = 1:num_sims, month = m,
                 note = "the real income level can only be cut, by Defend - Extend & Harvest must not touch it")
  }

  # ---- End-of-run audit ----------------------------------------------------
  # 1. Every nominal deposit must be its contractual real flow converted at that
  # month's factor. Built by two independent code paths; compared once, over the
  # whole [horizon x num_sims] grid.
  audit_check(audit, "deposit conversion",
              as.numeric(deposits_by_month),
              as.numeric(real_deposits * nfac[1:horizon, , drop = FALSE]),
              sim = rep(1:num_sims, each = horizon),
              note = "nominal deposit == contractual real flow x CPI(t)/CPI(t0)")

  # 2. The whole run, per path, as one ledger:
  #    closing equity + closing cash
  #      == opening equity + opening cash
  #       + equity gains + cash interest + bond coupons/redemptions
  #       - income paid - bonds bought after t0 + ladder liquidation proceeds
  # (equity -> cash ARVA transfers cancel out of the left-hand side, so they do
  # not appear; an extension's cash_top_up likewise nets against the equity it
  # was harvested from, leaving only bond_cost.)
  opening <- (pot - res_ilb_initial$total_outlay) + res_ilb_initial$cash_at_start
  audit_check(audit, "terminal reconciliation",
              EPort + cash_account,
              opening + acc_equity_gain + acc_cash_interest + acc_deposits -
                acc_income_paid - acc_bond_cost + acc_bond_liquidation,
              sim = 1:num_sims, month = horizon,
              note = "pot - bond cost + returns + coupons - spending, accumulated over the whole run")

  decision_log <- if (log_i > 0) do.call(rbind, log_rows[seq_len(log_i)]) else
    data.frame(sim = integer(0), year = numeric(0), action = character(0),
               funded_ratio = numeric(0), equity_return = numeric(0),
               withdrawal = numeric(0), ladder_years = numeric(0),
               bonds_bought = character(0), horizon_years = numeric(0),
               stringsAsFactors = FALSE)

  # Total wealth = equity + the cash account, at every month end. During the
  # ladder phase the bond sleeve is NOT included (it is not marked monthly - see
  # the per-path repricing note above); by the time any path terminates the
  # ladder has been liquidated into equity, so terminal wealth is complete.
  #
  # This is what ruin is now measured on. Measuring on EPort alone understated
  # terminal wealth badly, because ARVA progressively drains equity INTO the cash
  # account - the annuity factor approaches 1 as age approaches max_age, so the
  # final withdrawals move nearly everything across. On the base case that gap
  # was 80.6% vs 40.0% ruin: the same portfolio, scored twice, 40 points apart.
  Wealth_history <- EPort_history + Cash_history

  list(
    EPort_history           = EPort_history,
    Cash_history            = Cash_history,
    Wealth_history          = Wealth_history,
    Cash_deposit_history    = Cash_deposit_history,
    Cash_withdrawal_history = Cash_withdrawal_history,
    Cash_interest_history   = Cash_interest_history,
    Income_real_history     = Income_real_history,
    ARVA_withdrawal_history = ARVA_withdrawal_history,
    deposits_by_month       = deposits_by_month,
    real_deposits           = real_deposits,
    nominal_factor          = nfac,
    ladder_end_month        = ladder_end_month,
    ladder_years_final      = ladder_years_current,
    bond_sale_amount        = bond_sale_amount,
    ladder_locked           = ladder_locked,
    real_income_final       = real_income,
    cash_negative_months    = cash_negative_months,
    extension_bond_cost     = acc_bond_cost,
    extension_cash_topup    = acc_cash_topup,
    n_extensions            = n_extensions,
    arva_equity_drawn       = acc_arva_transfer,
    arva_floor_years        = arva_floor_years,
    n_unaffordable          = n_unaffordable,
    decision_log            = decision_log,
    EPort                   = EPort,
    Cash                    = cash_account,
    Wealth                  = EPort + cash_account
  )
}
