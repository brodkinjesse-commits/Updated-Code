# ==============================================================================
# Functions/Summarise_Run.R
# ==============================================================================
# Re-summarise a completed run WITHOUT re-running it.
#
# Thesis.R prints a full report at the end of every run and then throws the
# results away - there is no save step in it, so a 30-minute run leaves nothing
# behind but console text. Run_And_Save.R fixes that by saving a BUNDLE (the
# sim_result plus the parameters and metadata needed to interpret it); this file
# is what reads one back.
#
# Nothing here re-derives anything. Every figure below is computed exactly the
# way Thesis.R computes it, from the same fields - this is a reporting layer,
# not a second implementation. In particular:
#
#   - RUIN is measured on TOTAL WEALTH (equity + residual cash), against a
#     PER-PATH nominal target: the real target inflated by that path's own
#     realized CPI. Both halves matter. Scoring on equity alone understates
#     terminal wealth systematically (ARVA moves money out of equity into cash,
#     and the annuity factor approaches 1 near max_age), and scoring against a
#     flat nominal target would score a 77% erosion of the retiree's stated goal
#     as a success. The equity-only figure is printed alongside so the size of
#     the measurement effect stays visible.
#   - INCOME leads the report, because a decumulation strategy is judged first
#     on the income it pays. Income_real_history is already deflated to t0 rands
#     by each path's own CPI.
#
# Usage:
#   source("Functions/Summarise_Run.R")
#   summarise_run(readRDS("Runs/latest.rds"))     # a saved bundle
#   summarise_run(sim_result)                     # a live sim_result
#   res <- summarise_run(bundle); res$ruin        # values, for comparing arms
# ==============================================================================


# ------------------------------------------------------------------------------
# summarise_run()
# ------------------------------------------------------------------------------
# `x` is either a bundle from Run_And_Save.R (it has $sim_result) or a bare
# sim_result. With a bundle the parameters come from the run itself, so the
# summary can never be computed against parameters the run did not use - which
# is the whole reason for saving them together. With a bare sim_result you must
# supply any parameter that differs from the Thesis.R base case.
summarise_run <- function(x,
                          pot              = NULL,
                          wd               = NULL,
                          bequeathment_pct = NULL,
                          label            = NULL) {

  # ---- unwrap: bundle or bare sim_result ------------------------------------
  if (!is.null(x$sim_result)) {
    bundle     <- x
    sim_result <- x$sim_result
    p          <- x$params
    if (is.null(label)) label <- bundle$meta$label
  } else {
    bundle     <- NULL
    sim_result <- x
    p          <- list()
  }

  # Explicit arguments win, then the saved params, then the base case.
  pick <- function(arg, nm, default) {
    if (!is.null(arg))     return(arg)
    if (!is.null(p[[nm]])) return(p[[nm]])
    default
  }
  pot              <- pick(pot,              "pot",              10e6)
  wd               <- pick(wd,               "wd",               0.06)
  bequeathment_pct <- pick(bequeathment_pct, "bequeathment_pct", 0.10)
  if (is.null(label)) label <- "RUN SUMMARY"

  fmt <- function(v) format(round(v), big.mark = ",", scientific = FALSE)
  qs  <- c(0.05, 0.25, 0.50, 0.75, 0.95)
  qsl <- function(v) paste(fmt(quantile(v, qs)), collapse = " | ")

  # EPort_history / Cash_history carry an extra ROW 1 for the t0 opening
  # balance, so they are (horizon + 1) rows deep; the FLOW histories are exactly
  # horizon. Take the horizon off a flow history and assert the two agree -
  # reading it off EPort_history gave horizon = 361 and ran nominal_factor one
  # row past its end.
  horizon  <- nrow(sim_result$Income_real_history)
  num_sims <- ncol(sim_result$Income_real_history)
  stopifnot(nrow(sim_result$EPort_history)  == horizon + 1L,
            nrow(sim_result$nominal_factor) >= horizon + 1L)

  # Each path's cumulative CPI over the whole horizon. nominal_factor is the
  # engine's own copy of nfac_all, so this is identical to Thesis.R's
  # nfac_all[horizon + 1, ] - row k+1 is elapsed month k, so row horizon+1 is
  # month 360. Reading it off sim_result keeps this function self-contained: it
  # works in a fresh session with none of the model sourced.
  cpi_end <- sim_result$nominal_factor[horizon + 1, ]

  EPort  <- sim_result$EPort
  Cash   <- sim_result$Cash
  Wealth <- sim_result$Wealth          # == EPort + Cash

  # ---- ruin -----------------------------------------------------------------
  target_real <- bequeathment_pct * pot
  target_path <- target_real * cpi_end
  ruin        <- mean(Wealth < target_path)
  ruin_equity <- mean(EPort  < target_path)
  depletion   <- mean(Wealth <= 0)

  # ---- income ---------------------------------------------------------------
  income_real_m  <- sim_result$Income_real_history            # already t0 rands
  year_of_month  <- rep(seq_len(horizon %/% 12), each = 12)
  income_real_yr <- rowsum(income_real_m, group = year_of_month)
  target_income  <- wd * pot
  total_income   <- colSums(income_real_m)
  worst_year     <- apply(income_real_yr, 2, min)             # the income FLOOR

  # ---- print ----------------------------------------------------------------
  cat("\n=========================================================\n")
  cat(sprintf("  %s\n", label))
  cat(sprintf("  %d paths x %d months", num_sims, horizon))
  if (!is.null(bundle$meta$run_completed)) {
    cat(sprintf("   |   run %s", format(bundle$meta$run_completed, "%Y-%m-%d %H:%M")))
  }
  cat("\n=========================================================\n")

  if (!is.null(bundle$audit_ok)) {
    cat(sprintf("\nAccounting audit: %s\n",
                if (isTRUE(bundle$audit_ok)) "PASSED - all identities hold"
                else "*** FAILED - DO NOT QUOTE THESE NUMBERS ***"))
  }

  cat("\n-- INCOME DELIVERED (today's rands) ---------------------\n")
  cat(sprintf("target the ladder secures       : R%s p.a.\n", fmt(target_income)))
  cat(sprintf("total real income  5/25/50/75/95 : %s\n", qsl(total_income)))
  cat(sprintf("income FLOOR (worst year)        : %s\n", qsl(worst_year)))
  cat(sprintf("paths whose worst year cleared target : %.1f%%\n",
              100 * mean(worst_year >= target_income - 1e-6)))
  cat(sprintf("path-years paid below target          : %.1f%%\n",
              100 * mean(income_real_yr < target_income - 1e-6)))
  cat(sprintf("paths that never cut income           : %d / %d\n",
              sum(sim_result$real_income_final >= target_income - 1e-6), num_sims))

  cat("\n-- TERMINAL WEALTH --------------------------------------\n")
  cat(sprintf("nominal   median R%s | mean R%s | sd R%s\n",
              fmt(median(Wealth)), fmt(mean(Wealth)), fmt(sd(Wealth))))
  cat(sprintf("today's rands  5/25/50/75/95     : %s\n", qsl(Wealth / cpi_end)))
  cat(sprintf("split (nominal medians)          : equity R%s | residual cash R%s\n",
              fmt(median(EPort)), fmt(median(Cash))))
  cat(sprintf("paths ending with cash < 0       : %d / %d\n", sum(Cash < 0), num_sims))

  cat("\n-- RUIN -------------------------------------------------\n")
  cat(sprintf("target R%s in TODAY'S rands -> per-path nominal median R%s\n",
              fmt(target_real), fmt(median(target_path))))
  cat(sprintf("PROBABILITY OF RUIN (equity + residual cash) : %6.2f%%\n", 100 * ruin))
  cat(sprintf("  [equity-only reference measure]            : %6.2f%%\n", 100 * ruin_equity))
  cat(sprintf("Probability of complete depletion (<= R0)    : %6.2f%%\n", 100 * depletion))

  cat("\n-- LADDER / ARVA MECHANICS ------------------------------\n")
  cat(sprintf("extensions executed : %d (median %g per path) | unaffordable, skipped: %d\n",
              sum(sim_result$n_extensions), median(sim_result$n_extensions),
              sum(sim_result$n_unaffordable)))
  cat(sprintf("ladders locked      : %d / %d | median final length %g years\n",
              sum(sim_result$ladder_locked), num_sims,
              median(sim_result$ladder_years_final)))
  cat(sprintf("paths whose cash went negative at any point : %d / %d\n",
              sum(sim_result$cash_negative_months > 0), num_sims))
  arva_years <- sum(!is.na(sim_result$ARVA_withdrawal_history))
  cat(sprintf("ARVA years where the income floor bound     : %d of %d (%.1f%%)\n",
              sum(sim_result$arva_floor_years), arva_years,
              100 * sum(sim_result$arva_floor_years) / max(arva_years, 1)))
  cat("=========================================================\n\n")

  invisible(list(
    ruin              = ruin,
    ruin_equity_only  = ruin_equity,
    depletion         = depletion,
    wealth_nominal    = Wealth,
    wealth_real       = Wealth / cpi_end,
    target_path       = target_path,
    total_real_income = total_income,
    worst_year_real   = worst_year,
    income_real_yr    = income_real_yr
  ))
}


# ------------------------------------------------------------------------------
# compare_runs()
# ------------------------------------------------------------------------------
# Side-by-side table of two or more saved bundles - the dynamic strategy against
# a decisions_enabled = FALSE static arm, or one parameter swept. Takes a NAMED
# list of bundles (or bare sim_results).
compare_runs <- function(runs) {
  stopifnot(is.list(runs), length(runs) >= 1, !is.null(names(runs)))

  rows <- lapply(names(runs), function(nm) {
    r <- summarise_run(runs[[nm]], label = nm)
    data.frame(
      run              = nm,
      ruin_pct         = round(100 * r$ruin, 2),
      ruin_equity_pct  = round(100 * r$ruin_equity_only, 2),
      depletion_pct    = round(100 * r$depletion, 2),
      wealth_real_med  = round(median(r$wealth_real)),
      income_real_med  = round(median(r$total_real_income)),
      income_floor_med = round(median(r$worst_year_real)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  # Rand columns run to seven figures, which R prints as 6e+05 by default.
  # Suppressed for the print only - the returned frame stays numeric.
  old_opts <- options(scipen = 999); on.exit(options(old_opts), add = TRUE)

  cat("\n===================== RUN COMPARISON =====================\n")
  print(out, row.names = FALSE)
  cat("\n")
  invisible(out)
}
