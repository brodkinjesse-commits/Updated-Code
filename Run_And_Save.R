# ==============================================================================
# Run_And_Save.R  -  run Thesis.R once, then SAVE the results
# ==============================================================================
# WHY THIS EXISTS
#
# Thesis.R takes roughly 30 minutes for 1,000 paths and saves nothing. Every
# number it produces exists only as console text, so re-checking a single figure
# - or comparing the dynamic strategy against a static arm - means paying the
# full 30 minutes again. This script runs Thesis.R unchanged and then writes the
# results to disk as an .rds BUNDLE.
#
# The bundle is not just sim_result. It carries the parameters the run actually
# used, alongside the results those parameters produced. That pairing is the
# point: a summary computed against parameters the run did not use is exactly
# the class of silent error the invariant layer was built to prevent, and it is
# trivially easy to make by hand six weeks later.
#
# HOW TO RUN IT
#
#   From RStudio:  open this file and click Source  (Ctrl+Shift+S)
#   From a shell:  Rscript Run_And_Save.R
#   From R:        source("Run_And_Save.R")
#
# Either way the working directory must be the project root (the folder holding
# Thesis.R), because Thesis.R sources Functions/ by relative path. The guard
# below checks that before spending 30 minutes finding out.
#
# WHAT YOU GET
#
#   Runs/run_<label>_<timestamp>.rds   the permanent, timestamped record
#   Runs/latest.rds                    a copy of whichever ran most recently
#
# THEN, in any later session - no model sourced, no waiting:
#
#   source("Functions/Summarise_Run.R")
#   summarise_run(readRDS("Runs/latest.rds"))
#
# Runs/ is gitignored. The bundles are tens of MB and are run OUTPUT, not source.
# ==============================================================================


# ---- 0. Label this run -------------------------------------------------------
# Shows up in the filename and in the summary header. Change it when you sweep a
# parameter ("wd_7pct", "static_arm", "buffer_0") so the Runs/ folder stays
# readable instead of becoming a wall of timestamps.
RUN_LABEL <- "baseline"


# ---- 1. Locate the project root ----------------------------------------------
# Thesis.R calls source("Functions/...") with relative paths, so the working
# directory has to be the project root. here::here() finds it from the .git
# directory. Fail loudly and immediately if we are somewhere else - the
# alternative is a confusing error 30 seconds in, or worse, output written
# somewhere unexpected.
if (requireNamespace("here", quietly = TRUE)) {
  setwd(here::here())
}
if (!file.exists("Thesis.R")) {
  stop("Run_And_Save.R must be run from the project root (the folder containing ",
       "Thesis.R). Current working directory: ", getwd())
}


# ---- 2. Run the model --------------------------------------------------------
# Thesis.R is sourced UNMODIFIED. It prints its own full report as it goes -
# audit results, income, terminal wealth, ruin - and leaves every object it
# built in this environment. Nothing here changes any assumption; to change a
# parameter, change it in Thesis.R and set RUN_LABEL above to say so.
cat("\n#############################################################\n")
cat(sprintf("  Run_And_Save.R  |  label: %s\n", RUN_LABEL))
cat(sprintf("  started %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat("  Sourcing Thesis.R - this takes roughly 30 minutes for 1,000 paths.\n")
cat("#############################################################\n\n")

run_started <- Sys.time()
source("Thesis.R", echo = FALSE)
run_completed <- Sys.time()


# ---- 3. Assemble the bundle --------------------------------------------------
# sim_result holds the [360 x n_sims] histories and the per-path summaries.
# nfac_all is each path's realized CPI factor matrix - the bridge every
# real/nominal conversion in the reporting layer goes through. It is also
# carried inside sim_result$nominal_factor (the engine's own copy of the same
# matrix), which is what makes Summarise_Run.R work on a bare sim_result; it is
# saved here too so the deflation can be done directly without reaching into
# sim_result.
#
# res_ilb is the initial ladder solve - the bond purchase, the opening cash and
# its buffer/solved split, and the real schedule. Small, and it is what any
# question about WHAT WAS BOUGHT has to be answered from.
#
# audit_ok is saved deliberately. A bundle whose audit failed is a bundle whose
# numbers must not be quoted, and that fact belongs WITH the numbers rather than
# in a console log that scrolled away.
bundle <- list(
  sim_result = sim_result,
  nfac_all   = nfac_all,
  res_ilb    = res_ilb,
  audit_ok   = if (exists("audit_ok")) audit_ok else NA,

  # Every parameter the run actually used, saved next to the results it
  # produced. Summarise_Run.R reads pot / wd / bequeathment_pct from here.
  params = list(
    pot                      = pot,
    wd                       = wd,
    ladder_length            = ladder_length,
    max_ladder_years         = max_ladder_years,
    extend_by                = extend_by,
    defend_cut               = defend_cut,
    inflation_rate           = inflation_rate,
    cash_buffer_months       = cash_buffer_months,
    cash_annual_rate         = cash_annual_rate,
    bequeathment_pct         = bequeathment_pct,
    bequeathment_target_real = bequeathment_target_real,
    horizon                  = horizon,
    num_sims                 = num_sims,
    start_age                = start_age,
    max_age                  = max_age,
    valuation_date           = ILB_MODEL_START_DATE,
    cpi_t0                   = ILB_REFERENCE_CPI,
    decisions_enabled        = TRUE
  ),

  meta = list(
    label         = RUN_LABEL,
    run_started   = run_started,
    run_completed = run_completed,
    elapsed       = difftime(run_completed, run_started, units = "mins"),
    r_version     = R.version.string,
    git_commit    = tryCatch(
      trimws(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)),
      error = function(e) NA_character_,
      warning = function(w) NA_character_
    )
  )
)


# ---- 4. Write it out ---------------------------------------------------------
# Two files: a timestamped one that is never overwritten (the permanent record
# of this run) and latest.rds (the convenient handle). Timestamp first in the
# name so the folder sorts chronologically.
if (!dir.exists("Runs")) dir.create("Runs")

stamp     <- format(run_completed, "%Y%m%d_%H%M%S")
run_file  <- file.path("Runs", sprintf("run_%s_%s.rds", stamp, RUN_LABEL))
last_file <- file.path("Runs", "latest.rds")

saveRDS(bundle, run_file)
saveRDS(bundle, last_file)

cat("\n#############################################################\n")
cat("  RESULTS SAVED\n")
cat(sprintf("    %s   (%.1f MB)\n", run_file, file.size(run_file) / 1024^2))
cat(sprintf("    %s   (same bundle, convenience handle)\n", last_file))
cat(sprintf("  elapsed: %.1f minutes\n", as.numeric(bundle$meta$elapsed)))
if (!isTRUE(bundle$audit_ok)) {
  cat("\n  *** WARNING: the accounting audit did not pass. ***\n")
  cat("  *** Do not quote these numbers until it does.   ***\n")
}
cat("\n  To re-summarise later, in a fresh session:\n")
cat("    source(\"Functions/Summarise_Run.R\")\n")
cat("    summarise_run(readRDS(\"Runs/latest.rds\"))\n")
cat("#############################################################\n\n")


# ---- 5. Immediate summary ----------------------------------------------------
# Prove the bundle round-trips: read it back off disk and summarise THAT, not
# the in-memory copy. If anything failed to serialise, this is where it shows,
# while the 30 minutes of work is still recoverable from the session.
source("Functions/Summarise_Run.R")
summarise_run(readRDS(last_file))
