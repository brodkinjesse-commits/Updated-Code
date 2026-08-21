# ==============================================================================
# INVARIANTS: accounting identities, boundary checks, and the run audit
# ==============================================================================
# This file exists because five separate defects in this model were all the same
# mistake - a REAL rand amount used where a NOMINAL one was required - and every
# one of them was silent. Nothing crashed, no number looked absurd, and the run
# produced a clean-looking distribution that was wrong by up to 1.8x. The point
# of what follows is to convert that class of failure from silent corruption
# into a loud, located failure.
#
# Three layers, cheapest and loudest first:
#
#   1. BOUNDARY CHECKS (run_boundary_checks)
#      Run BEFORE the simulation, on the t0 ladder, and stop() on failure. They
#      are the two anchors that pin the real <-> nominal conversion in place:
#
#        a. reprice_ladder(month = 0) must equal the selector's own total_cost.
#           At t0 there is no inflation and no curve movement, so marking the
#           ladder must return exactly what was paid for it. This single line
#           catches any unit slip in the mark: the pre-fix code returned
#           R6,127,443 against a total_cost of R3,404,649 - a 1.80x
#           over-statement, at t0, before a single month had passed.
#
#        b. With inflation switched off, nominal_deposits_for_holdings() must
#           reproduce the selector's own real cash-flow schedule exactly. If
#           CPI never moves, nominal IS real, so any difference is a unit error
#           in the conversion rather than an economic one. The pre-fix code
#           failed this by each bond's t0 index ratio (1.14x - 3.19x).
#
#      Neither costs anything to run and both are unconditional. A corrupted run
#      never starts.
#
#   2. THE PER-(PATH, MONTH) AUDIT (audit_new / audit_check / audit_report)
#      Accumulated inside the simulation loop, reported once at the end. Nothing
#      stops mid-run: a stray path aborting a 1,000-path, 25-minute job costs
#      more than it saves, and a breach on one path is usually a breach on all of
#      them. What is recorded per check is the count, the worst absolute and
#      relative breach, and the exact (sim, month) it happened at, so a failure
#      is immediately reproducible on a single path.
#
#      The identities checked are listed against each audit_check() call site in
#      Functions/Dynamic_Ladder.R. The load-bearing ones:
#        cash        opening x (1+i) + deposits - withdrawals == closing
#        deposits    nominal deposit == real contractual flow x CPI(t)/CPI(t0)
#        purchase    total assets immediately before an extension == immediately
#                    after (a purchase moves money between sleeves; it cannot
#                    create or destroy any)
#        liability   the funded ratio's first-year expense == 12 x the nominal
#                    payment actually being made this month
#        income      the real income level never rises (it can only be cut, by
#                    Defend) - a direct guard on the income mechanic
#        terminal    EPort + cash at the end == pot - bond cost + returns
#                    + coupons - spending, per path, over all 360 months
#
#   3. THE GOLDEN PATH (boundary_check_golden_path)
#      An end-to-end check, and the strongest of the three. Run the whole engine
#      on ONE deterministic path - inflation exactly equal to the assumed rate,
#      decisions disabled - and deflate its cash ledger back to t0 rands. It must
#      reproduce the selector's own real schedule, month for month, for the
#      entire initial ladder. Every conversion, the withdrawal indexation, the
#      deposit schedule and the cash mechanics all have to be simultaneously
#      right for this to pass. It is the check that would have caught all five
#      defects on its own.
#
# TOLERANCES: a breach is anything larger than tol_abs + tol_rel * scale, where
# scale is the larger of the two sides. The defaults (1e-6 absolute, 1e-8
# relative) are far tighter than any defect this is guarding against - on a R10m
# portfolio 1e-8 relative is 10 cents - and loose enough to absorb ordinary
# floating-point accumulation over 360 months.
# ==============================================================================

# ------------------------------------------------------------------------------
# audit_new()
# ------------------------------------------------------------------------------
# An accumulator. An environment rather than a list because it is written to from
# inside the simulation loop and R would otherwise copy it on every update, which
# on 1,000 paths x 360 months is the difference between free and not.
#
# enabled = FALSE turns every audit_check() into an immediate return, so the
# audit can be switched off for a production run without touching call sites.
audit_new <- function(enabled = TRUE, tol_abs = 1e-6, tol_rel = 1e-8) {
  a <- new.env(parent = emptyenv())
  a$enabled  <- isTRUE(enabled)
  a$tol_abs  <- tol_abs
  a$tol_rel  <- tol_rel
  a$checks   <- list()      # name -> list(n, n_fail, max_abs, max_rel, where, note)
  a$order    <- character(0)
  class(a)   <- "ladder_audit"
  a
}

# ------------------------------------------------------------------------------
# audit_check()
# ------------------------------------------------------------------------------
# Assert lhs == rhs. Both may be vectors (e.g. all paths for one month at once),
# in which case `sim` may be a matching vector naming which path each element is.
#
#   name  the identity being checked - reused across call sites to aggregate.
#   sim   path index (scalar or vector, recycled).
#   month simulation month (scalar).
#   note  free text stored with the worst breach, for context in the report.
audit_check <- function(audit, name, lhs, rhs, sim = NA_integer_, month = NA_integer_,
                        note = "") {
  if (is.null(audit) || !audit$enabled) return(invisible(NULL))

  lhs <- as.numeric(lhs); rhs <- as.numeric(rhs)
  n   <- max(length(lhs), length(rhs))
  if (n == 0L) return(invisible(NULL))
  lhs <- rep_len(lhs, n); rhs <- rep_len(rhs, n)
  sim <- rep_len(as.numeric(sim), n)

  d      <- abs(lhs - rhs)
  scale  <- pmax(abs(lhs), abs(rhs))
  limit  <- audit$tol_abs + audit$tol_rel * scale
  fail   <- is.na(d) | (d > limit)
  relerr <- ifelse(scale > 0, d / scale, d)

  rec <- audit$checks[[name]]
  if (is.null(rec)) {
    rec <- list(n = 0, n_fail = 0, max_abs = 0, max_rel = 0,
                worst_sim = NA_real_, worst_month = NA_real_,
                worst_lhs = NA_real_, worst_rhs = NA_real_, note = note)
    audit$order <- c(audit$order, name)
  }

  rec$n      <- rec$n + n
  rec$n_fail <- rec$n_fail + sum(fail, na.rm = TRUE)

  if (any(fail, na.rm = TRUE)) {
    i <- which.max(ifelse(is.na(d), Inf, d))
    if (d[i] > rec$max_abs || is.na(rec$worst_sim)) {
      rec$max_abs     <- d[i]
      rec$max_rel     <- relerr[i]
      rec$worst_sim   <- sim[i]
      rec$worst_month <- month
      rec$worst_lhs   <- lhs[i]
      rec$worst_rhs   <- rhs[i]
      rec$note        <- note
    }
  }

  audit$checks[[name]] <- rec
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# audit_assert()
# ------------------------------------------------------------------------------
# The same accumulator for conditions that are not a numeric equality (e.g. "the
# real income never rises"). `ok` is a logical vector; FALSE is a breach.
audit_assert <- function(audit, name, ok, sim = NA_integer_, month = NA_integer_,
                         note = "") {
  if (is.null(audit) || !audit$enabled) return(invisible(NULL))
  ok  <- as.logical(ok)
  n   <- length(ok)
  if (n == 0L) return(invisible(NULL))
  sim <- rep_len(as.numeric(sim), n)

  rec <- audit$checks[[name]]
  if (is.null(rec)) {
    rec <- list(n = 0, n_fail = 0, max_abs = 0, max_rel = 0,
                worst_sim = NA_real_, worst_month = NA_real_,
                worst_lhs = NA_real_, worst_rhs = NA_real_, note = note)
    audit$order <- c(audit$order, name)
  }
  bad <- is.na(ok) | !ok
  rec$n      <- rec$n + n
  rec$n_fail <- rec$n_fail + sum(bad)
  if (any(bad) && is.na(rec$worst_sim)) {
    rec$worst_sim   <- sim[which(bad)[1]]
    rec$worst_month <- month
    rec$note        <- note
  }
  audit$checks[[name]] <- rec
  invisible(NULL)
}

audit_failed <- function(audit) {
  if (is.null(audit) || !audit$enabled) return(FALSE)
  any(vapply(audit$checks, function(r) r$n_fail > 0, logical(1)))
}

# ------------------------------------------------------------------------------
# audit_report()
# ------------------------------------------------------------------------------
# One table, printed after the run. A PASS line means the identity held for every
# (path, month) it was evaluated at - the count is shown so a check that silently
# stopped running (n = 0) is as visible as one that failed.
audit_report <- function(audit, header = "SIMULATION AUDIT") {
  cat("\n=======================================================\n")
  cat(sprintf("  %s\n", header))
  cat("=======================================================\n")

  if (is.null(audit) || !audit$enabled) {
    cat("  audit DISABLED - no identities were checked this run.\n")
    cat("=======================================================\n")
    return(invisible(FALSE))
  }
  if (length(audit$order) == 0) {
    cat("  no checks were recorded - the audit was passed in but never called.\n")
    cat("=======================================================\n")
    return(invisible(FALSE))
  }

  cat(sprintf("  tolerance: %.1e absolute + %.1e relative\n\n", audit$tol_abs, audit$tol_rel))

  for (nm in audit$order) {
    r <- audit$checks[[nm]]
    if (r$n_fail == 0) {
      cat(sprintf("  PASS  %-34s %10s checks\n", nm, format(r$n, big.mark = ",")))
    } else {
      cat(sprintf("  FAIL  %-34s %10s checks, %s breached\n",
                  nm, format(r$n, big.mark = ","), format(r$n_fail, big.mark = ",")))
      cat(sprintf("        worst: sim %s, month %s | %.6g vs %.6g\n",
                  format(r$worst_sim), format(r$worst_month), r$worst_lhs, r$worst_rhs))
      cat(sprintf("        breach: %.6g absolute, %.3g%% relative\n",
                  r$max_abs, 100 * r$max_rel))
      if (nzchar(r$note)) cat(sprintf("        note: %s\n", r$note))
    }
  }

  failed <- audit_failed(audit)
  cat("\n")
  if (failed) {
    cat("  RESULT: FAILED - the numbers above this line cannot be trusted.\n")
    cat("  Reproduce on the single worst path before reading any result.\n")
  } else {
    cat("  RESULT: all identities held on every path, every month.\n")
  }
  cat("=======================================================\n")
  invisible(!failed)
}

# ==============================================================================
# BOUNDARY CHECKS - run before the simulation, stop() on failure
# ==============================================================================

# ------------------------------------------------------------------------------
# boundary_check_reprice_t0()
# ------------------------------------------------------------------------------
# Marking the ladder at t0 must return exactly what was paid for it. There is no
# inflation to apply and no curve movement to reprice against, so the two sides
# are the same trade valued the same day.
#
# This is the check that fails the moment anyone converts a per-UNIT quantity
# with a per-R100-BASE-PAR factor. Before the fix it returned R6,127,443 against
# a total_cost of R3,404,649.
boundary_check_reprice_t0 <- function(res_ilb, bonds, valuation_date, reference_cpi,
                                      cpi_t0 = ILB_REFERENCE_CPI, tol_rel = 1e-9) {

  marked   <- reprice_ladder(res_ilb$bond_units, bonds, valuation_date,
                             month = 0, sim = 1, real_yc = NULL,
                             reference_cpi = reference_cpi, cpi_t0 = cpi_t0)
  marked_v <- sum(marked$rand_value)
  cost     <- res_ilb$total_cost

  if (abs(marked_v - cost) > tol_rel * max(abs(cost), 1)) {
    stop(sprintf(paste0("BOUNDARY CHECK FAILED - reprice_ladder(month = 0) does not equal the ",
                        "ladder's purchase cost.\n",
                        "  marked at t0 : R %s\n",
                        "  total_cost   : R %s\n",
                        "  ratio        : %.6f\n",
                        "Marking the ladder on the day it was bought must return what was paid ",
                        "for it. A ratio close to a bond index ratio (1.14 - 3.19 on this ",
                        "universe) means a per-unit amount is being converted with a ",
                        "per-R100-base-par factor - see the units note in ",
                        "Functions/ILB_Repricing.R. The simulation has NOT been run."),
                 format(round(marked_v, 2), big.mark = ","),
                 format(round(cost, 2),     big.mark = ","),
                 marked_v / cost))
  }

  invisible(list(marked = marked_v, cost = cost))
}

# ------------------------------------------------------------------------------
# boundary_check_deposits_vs_schedule()
# ------------------------------------------------------------------------------
# With inflation switched off, nominal IS real. So feeding a flat CPI path
# through nominal_deposits_for_holdings() must reproduce, month for month, the
# bond_inflow column of the selector's own real schedule - which was built by a
# completely separate code path (ilb_ladder_real_schedule()) from the same
# contractual cash flows.
#
# Any difference is therefore a unit or bucketing error in the conversion rather
# than an economic difference. The pre-fix code failed this by each bond's own t0
# index ratio.
boundary_check_deposits_vs_schedule <- function(res_ilb, bonds, valuation_date,
                                                cpi_t0 = ILB_REFERENCE_CPI,
                                                tol = 1e-6) {

  n_months <- res_ilb$schedule$n_months
  flat_cpi <- rep(cpi_t0, n_months)   # zero inflation: every factor is exactly 1

  dep <- nominal_deposits_for_holdings(
    res_ilb$bond_units, bonds,
    purchase_date         = valuation_date,
    global_valuation_date = valuation_date,
    reference_cpi_col     = flat_cpi,
    horizon               = n_months,
    cpi_t0                = cpi_t0
  )

  sched <- res_ilb$schedule$schedule$bond_inflow
  d     <- abs(dep - sched)
  bad   <- which(d > tol + 1e-9 * pmax(abs(dep), abs(sched)))

  if (length(bad)) {
    worst <- bad[which.max(d[bad])]
    stop(sprintf(paste0("BOUNDARY CHECK FAILED - with inflation switched off, the engine's nominal ",
                        "deposit schedule does not match the selector's own real schedule.\n",
                        "  months breached : %d of %d\n",
                        "  worst month     : %d\n",
                        "  deposits        : R %s\n",
                        "  selector        : R %s\n",
                        "  ratio           : %.6f\n",
                        "If CPI never moves, nominal IS real, so these must be identical. A ratio ",
                        "near a bond index ratio means nominal_bond_deposits() is converting with ",
                        "the index ratio instead of CPI(t)/CPI(t0) - see the header of ",
                        "Functions/ILB_Indexation.R. The simulation has NOT been run."),
                 length(bad), n_months, worst,
                 format(round(dep[worst], 2),   big.mark = ","),
                 format(round(sched[worst], 2), big.mark = ","),
                 dep[worst] / max(sched[worst], 1e-12)))
  }

  invisible(list(deposits = dep, schedule = sched))
}

# ------------------------------------------------------------------------------
# check_inflation_assumption()
# ------------------------------------------------------------------------------
# A SOFT check - it reports and warns, it does not stop. Whether the flat
# `inflation_rate` should equal the historical mean is a modelling judgement,
# not an accounting identity, so this surfaces the comparison and leaves the
# call to the reader.
#
# It is here because the last time the two disagreed it was expensive and
# invisible. inflation_rate was 5.0% while the bootstrap it is paired with
# delivers 5.78%, and that single parameter feeds two things:
#
#   1. optimize_ilb_ladder()'s Fisher real rate on idle cash. The solve is
#      exactly binding (min_balance = 0), so a real rate assumed 0.78pp too high
#      is not absorbed anywhere - it goes straight into overdraft. Measured:
#      86.5% of static base-case ladders ran the cash account negative.
#   2. compute_funded_ratio()'s forward liability escalation, where too low a
#      rate understates every future expense and biases the annual review toward
#      Extend & Harvest.
#
# The warning threshold is deliberately generous (25 bp): this is meant to catch
# a parameter that has drifted away from its data, not to police a deliberate
# forward-looking view that differs from history.
check_inflation_assumption <- function(inflation_monthly_ratios, inflation_rate,
                                       tol_pp = 0.0025, verbose = TRUE) {

  horizon   <- nrow(inflation_monthly_ratios)
  path_ann  <- apply(inflation_monthly_ratios, 2, function(x) prod(x)^(12 / horizon) - 1)
  realised  <- mean(path_ann)
  gap       <- realised - inflation_rate

  if (verbose) {
    cat("\n--- inflation assumption vs simulated inflation -------------------\n")
    cat(sprintf("  assumed inflation_rate        : %.3f%% p.a.\n", 100 * inflation_rate))
    cat(sprintf("  bootstrapped paths, mean      : %.3f%% p.a.  (median %.3f%%)\n",
                100 * realised, 100 * median(path_ann)))
    cat(sprintf("  paths above the assumption    : %.1f%%\n", 100 * mean(path_ann > inflation_rate)))
    cat(sprintf("  implied real cash rate: assumed %+.3f%%, realised %+.3f%% (mean)\n",
                100 * ((1 + 0.05) / (1 + inflation_rate) - 1),
                100 * mean((1 + 0.05) / (1 + path_ann) - 1)))
    cat(sprintf("  %s  gap of %+.3f pp\n",
                if (abs(gap) > tol_pp) "WARN " else "OK   ", 100 * gap))
    cat("------------------------------------------------------------------\n")
  }

  if (abs(gap) > tol_pp) {
    warning(sprintf(paste0("inflation_rate (%.3f%%) differs from the inflation the bootstrap actually ",
                           "delivers (%.3f%% mean) by %+.3f pp. This parameter sets the Fisher real ",
                           "rate on the ladder's cash AND escalates the funded ratio's forward ",
                           "liability, so a gap here biases both. Set it deliberately or not at all."),
                   100 * inflation_rate, 100 * realised, 100 * gap))
  }

  invisible(list(assumed = inflation_rate, realised = realised, gap = gap,
                 path_annualised = path_ann))
}

# ------------------------------------------------------------------------------
# run_boundary_checks()
# ------------------------------------------------------------------------------
# Both of the above plus the CPI anchor assertion, as one call. Put this
# immediately after the initial optimize_ilb_ladder() and before the simulation.
run_boundary_checks <- function(res_ilb, bonds, valuation_date, reference_cpi,
                                cpi_anchor, cpi_t0 = ILB_REFERENCE_CPI, verbose = TRUE) {

  assert_cpi_anchor(cpi_anchor, cpi_t0)
  a <- boundary_check_reprice_t0(res_ilb, bonds, valuation_date, reference_cpi, cpi_t0)
  b <- boundary_check_deposits_vs_schedule(res_ilb, bonds, valuation_date, cpi_t0)

  if (verbose) {
    cat("\n--- t0 boundary checks -------------------------------------------\n")
    cat(sprintf("  PASS  CPI anchor == ILB_REFERENCE_CPI          %.4f\n", cpi_t0))
    cat(sprintf("  PASS  reprice_ladder(month = 0) == total_cost  R %s\n",
                format(round(a$cost, 2), big.mark = ",")))
    cat(sprintf("  PASS  zero-inflation deposits == schedule      %d months, R %s total\n",
                length(b$schedule), format(round(sum(b$schedule), 2), big.mark = ",")))
    cat("------------------------------------------------------------------\n")
  }

  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# boundary_check_golden_path()
# ------------------------------------------------------------------------------
# The end-to-end check. Run the real engine on ONE synthetic path where:
#   - realized inflation is exactly `inflation_rate` every month, so the Fisher
#     real rate the selector assumed is the real rate the engine actually earns;
#   - the decision matrix is disabled, so the ladder is never extended and the
#     income level never changes;
#   - equity returns are flat (they are irrelevant - nothing touches equity).
#
# Under those conditions the engine's nominal cash ledger, deflated back to t0
# rands, MUST equal the selector's own real schedule balance, month for month,
# for the whole initial ladder. That single comparison exercises the deposit
# conversion, the withdrawal indexation, the cash mechanics and their ordering
# all at once.
#
# Returns a data.frame of the comparison invisibly, and stops on breach.
boundary_check_golden_path <- function(res_ilb, bonds, valuation_date,
                                       pot, wd, ladder_length, cash_annual_rate,
                                       inflation_rate, start_age, max_age,
                                       yc, real_yc, cpi_t0 = ILB_REFERENCE_CPI,
                                       tol_rel = 1e-8, verbose = TRUE) {

  n_months <- res_ilb$schedule$n_months
  horizon  <- n_months

  monthly_ratio <- (1 + inflation_rate)^(1 / 12)
  infl_1        <- matrix(monthly_ratio, nrow = horizon, ncol = 1)
  ref_cpi_1     <- reference_cpi_path(infl_1, cpi_t0)
  equity_1      <- matrix(1, nrow = horizon, ncol = 1)   # flat: equity is untouched here

  gold <- run_dynamic_ladder_simulation(
    pot = pot, wd = wd,
    ladder_length = ladder_length, max_ladder_years = ladder_length, extend_by = 0,
    ilb_bonds = bonds, valuation_date = valuation_date,
    equity_monthly_returns = equity_1, horizon = horizon,
    cash_annual_rate = cash_annual_rate, defend_cut = 0,
    inflation_rate = inflation_rate,
    start_age = start_age, max_age = max_age,
    res_ilb_initial = res_ilb,
    yc = yc, real_yc = real_yc, reference_cpi = ref_cpi_1,
    cpi_t0 = cpi_t0,
    decisions_enabled = FALSE,
    audit = NULL
  )

  # Deflate the nominal ledger back to t0 rands.
  #
  # WHICH FACTOR: Cash_history row m+1 is the closing balance of simulation
  # month m, but every event inside that month happens at its START - interest
  # is credited on the opening balance, then the annuity-due withdrawal goes out
  # and the month's coupons come in, and nothing else touches the account before
  # the next month opens. The closing balance is therefore denominated in
  # month-m-START rands, i.e. ELAPSED MONTH m-1, and deflates at factor(m-1).
  #
  # Getting this wrong is worth exactly one month of inflation - 0.407% at 5% -
  # which is small enough to look like rounding and large enough to be a real
  # error. It is also precisely the kind of off-by-one this whole file exists to
  # refuse to let pass silently.
  f_open    <- nominal_factor_matrix(ref_cpi_1, cpi_t0)[1:horizon, 1]
  cash_nom  <- gold$Cash_history[2:(horizon + 1), 1]
  cash_real <- cash_nom / f_open
  sched_bal <- res_ilb$schedule$schedule$balance

  cmp <- data.frame(month = 1:horizon,
                    engine_real   = cash_real,
                    selector_real = sched_bal,
                    diff          = cash_real - sched_bal)

  scale <- pmax(abs(cash_real), abs(sched_bal), res_ilb$cash_at_start)
  bad   <- which(abs(cmp$diff) > tol_rel * scale + 1e-6)

  if (length(bad)) {
    worst <- bad[which.max(abs(cmp$diff[bad]))]
    stop(sprintf(paste0("GOLDEN PATH CHECK FAILED - the engine's deflated cash ledger does not ",
                        "reproduce the selector's real schedule.\n",
                        "  months breached : %d of %d\n",
                        "  worst month     : %d\n",
                        "  engine (real)   : R %s\n",
                        "  selector (real) : R %s\n",
                        "  difference      : R %s\n",
                        "On a path where realized inflation equals the assumed rate and no ",
                        "decision ever fires, these are the same bank account described twice. ",
                        "A drift that GROWS with the month index is a conversion error; a ",
                        "constant offset is an ordering or opening-balance error. The full ",
                        "simulation has NOT been run."),
                 length(bad), horizon, worst,
                 format(round(cmp$engine_real[worst], 2),   big.mark = ","),
                 format(round(cmp$selector_real[worst], 2), big.mark = ","),
                 format(round(cmp$diff[worst], 2),          big.mark = ",")))
  }

  if (verbose) {
    cat("\n--- golden path (deterministic, decisions off) --------------------\n")
    cat(sprintf("  PASS  deflated cash ledger == selector schedule, all %d months\n", horizon))
    cat(sprintf("        max drift over the ladder: R %.6f\n", max(abs(cmp$diff))))
    cat(sprintf("        closing real balance: engine R %s | selector R %s\n",
                format(round(cash_real[horizon], 2), big.mark = ","),
                format(round(sched_bal[horizon], 2), big.mark = ",")))
    cat("------------------------------------------------------------------\n")
  }

  invisible(cmp)
}
