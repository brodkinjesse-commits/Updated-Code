library(readxl)
library(dplyr)
library(here)

# Source order matters. Bond Selection ILB.R and ARVA.R both run code at source
# time and create globals the rest depends on (ilb_fixed, ILB_MODEL_START_DATE,
# ILB_REFERENCE_CPI, and the mortality table `data`), and ILB_Repricing.R /
# ILB_Indexation.R call functions defined in Bond Selection ILB.R.
# Directory names are Functions/ and Data/ - cased exactly, so this runs on a
# case-sensitive filesystem too.
source("Functions/moving_block_bootstrap.R")
source("Functions/Bond Selection ILB.R")
source("Functions/ARVA.R")
source("Functions/Decision_Matrix.R")
source("Functions/Dynamic_Ladder.R")
source("Functions/yield_curve_simulation.R")
source("Functions/Real_Yield_Curve.R")
source("Functions/ILB_Indexation.R")
source("Functions/ILB_Repricing.R")
source("Functions/Invariants.R")

# Timing --------------------------------------------------------------------
# Overall script clock - started as early as possible (right after the
# library/source calls, before any data loading) so the total includes
# everything: data import, bootstrapping, the dynamic ladder simulation, and
# all the summary/plotting code below. Printed at the very end of the script.
script_start_time <- Sys.time()

# Data --------------------------------------------------------------------
# --- 1. Import data ---
Bond_Equity_Data <- read_excel(here("Data", "Bond & Equity Data.xlsx"), sheet = "LT_DATA", skip = 1)
CPI_Data <- read_excel(here("Data", "CPI_Index_Long_Format.xlsx"))
Yield_Data <- read_excel(here("Data", "SA Nominal Yield Curves - 1989-2026.xlsx"))

# --- 2. Clean Bond_Equity_Data ---
colnames(Bond_Equity_Data)[1] <- "Date"
colnames(Bond_Equity_Data) <- make.names(colnames(Bond_Equity_Data))
Bond_Equity_Data$Date <- as.Date(Bond_Equity_Data$Date)

# --- 3. Trim to usable range and compute equity/bond returns ---
Bond_Equity_Data <- na.omit(Bond_Equity_Data[-1:-407, 1:3])
prices <- Bond_Equity_Data[, c("SA.Equity.ZAR", "SA.Bonds.ZAR")]
temp_returns <- prices[-1, ] / prices[-nrow(prices), ]

BE_Returns <- data.frame(
  YearMon       = format(Bond_Equity_Data$Date[-1], "%Y-%m"),
  SA.Equity.ZAR = temp_returns$SA.Equity.ZAR,
  SA.Bonds.ZAR  = temp_returns$SA.Bonds.ZAR
)

# --- 4. CPI cleaning / inflation growth construction ---
CPI_sub <- CPI_Data[168:558, ]  # Dec 1993 to end
cpi_dates <- as.Date(paste0("01 ", CPI_sub$Date), format = "%d %b %Y")
cpi_values <- CPI_sub$`CPI Index`

inf_growth_ratios <- cpi_values[-1] / cpi_values[-length(cpi_values)]

Inf_index_clean <- data.frame(
  YearMon    = format(cpi_dates[-1], "%Y-%m"),
  Inf_Growth = inf_growth_ratios
)

# --- 5. Merge equity/bond returns with inflation on YearMon ---
combined_data <- merge(BE_Returns, Inf_index_clean, by = "YearMon")
combined_data_clean <- combined_data[complete.cases(combined_data), ]

returns_matrix <- as.matrix(combined_data_clean[, c("SA.Equity.ZAR", "SA.Bonds.ZAR", "Inf_Growth")])

# Bootstrapping -----------------------------------------------------------
set.seed(392)
bootstrap_start_time <- Sys.time()

sim_paths = moving_block_bootstrap(returns_matrix,
                                   block_size = 6,
                                   horizon = 360,
                                   n_sims = 1000)

bootstrap_end_time <- Sys.time()
cat(sprintf("\nBootstrap runtime: %s\n", format(bootstrap_end_time - bootstrap_start_time)))

dim(sim_paths)
head(returns_matrix)

#equity_path = sim_paths[, "SA.Equity.ZAR", 1]
#plot(cumprod(equity_path), type = "l", xlab = "Month", ylab = "Cumulative Growth (Sim 1)", main = "Simulated 30-Year SA Equity Path")

#Cumulative growth factor for one simulation's equity sleeve
#equity_growth = apply(sim_paths[, "SA.Equity.ZAR", ], 2, cumprod)
#bond_growth   = apply(sim_paths[, "SA.Bonds.ZAR", ], 2, cumprod)

#Actual monthly returns for the equity sleeve (all sims)
equity_monthly_returns = sim_paths[, "SA.Equity.ZAR", ]   # [horizon x n_sims], raw period returns
bond_monthly_returns   = sim_paths[, "SA.Bonds.ZAR", ]    # same, raw period returns
inflation_monthly_ratios = sim_paths[, "Inf_Growth", ]    # [horizon x n_sims], raw monthly inflation growth ratios

# Parameters
pot <- 10000000
wd <- 0.06                # Bond ladder withdrawal rate the retiree wants to secure each year
ladder_length <- 8        # STARTING ladder length - can extend annually up to max_ladder_years
max_ladder_years <- 15    # Hard cap - reaching it forces liquidation into equity, unconditionally
extend_by <- 1            # Years added per Extend & Harvest decision (kept as a parameter, per request)
defend_cut <- 0.05        # Withdrawal cut applied on a Defend year (passed through to decision_matrix())
inflation_rate <- 0.058   # Flat EXPECTED rate - still used for compute_funded_ratio()'s forward
# liability projection and decision_matrix()'s annual raise (both are
# forward-looking, so can't discount against inflation that hasn't
# happened yet). The actual monthly cash withdrawal is now indexed to
# the REALIZED bootstrapped inflation_monthly_ratios path instead (see
# run_dynamic_ladder_simulation() call below), not this flat number.
#
# WHY 5.8% AND NOT 5%: this parameter has to be consistent with the CPI series
# the model bootstraps, and it was not. The series (Dec 1993 - Jun 2026) has a
# geometric mean of 5.817% p.a. and the bootstrapped paths reproduce 5.76%, but
# inflation_rate was set to 5.0%. That mattered in two places:
#   1. optimize_ilb_ladder() derives the real rate credited on idle cash from it
#      via Fisher. At 5% the solver assumed 0.00% real; paths actually earned
#      -0.74% real, and 86.5% of them earned less than assumed. Since the cash
#      solve is exactly binding (min_balance = 0), that drift alone put 86.5% of
#      static ladders into overdraft.
#   2. compute_funded_ratio() escalates the forward liability at it, so a low
#      rate understates every future expense and biases the annual review toward
#      Extend & Harvest - visible in the decision logs as funded ratios of
#      1.2-1.45 on paths that end far below target.
# check_inflation_assumption() below re-derives the realised figure from the
# bootstrap every run and warns if the two drift apart again.
cash_buffer_months <- 2   # Stated margin on the ILB ladder's opening cash, in months
# of real income, ON TOP of the binding minimum the solve produces
# (see optimize_ilb_ladder()). The solve is exactly binding by
# construction, so it has no tolerance for the real-rate drift above.
# Sized on the base case: +1 month leaves 10.5% of static paths short,
# +2 months leaves 0.0%, at a cost of ~1% of the pot diverted from
# equity. Applied to the INITIAL purchase only. Extensions are not
# separately buffered: the engine sizes each one against the cash its
# pooled account is projected to still be holding on the sub-ladder's
# start date, so the t0 margin is carried forward and re-used rather
# than a fresh margin being stacked on top of it every year.
bequeathment_pct <- 0.10  # Retiree-set: fraction of the starting pot they want left over at the end
# of the horizon - in REAL terms. See bequeathment_target_real below.
cash_annual_rate <- 0.05  # effective annual rate, compounded monthly - also the nominal_interest_rate
# fed to optimize_ilb_ladder()'s Fisher real-rate calc (Functions/Bond Selection ILB.R),
# so needed before the bond ladder section below, not after it as in the old script order.
horizon <- 360
num_sims <- ncol(equity_monthly_returns)

# The portfolio is a "success" for this retiree if it ends the horizon with at
# least this much left, rather than merely surviving above R0.
#
# THE TARGET IS REAL. R1,000,000 of TODAY'S purchasing power, not R1,000,000 of
# month-360 rands - those are not the same bequest, and at 5% inflation the
# nominal figure is worth R231,377 in today's money by the end of the horizon.
# Comparing a nominal terminal portfolio against a nominal-fixed target scored a
# 77% erosion of the retiree's stated goal as a success.
#
# The per-path nominal target is therefore this figure inflated by THAT PATH'S
# OWN realized CPI over the horizon (built after the run, once reference_cpi is
# to hand) - so a high-inflation path is judged against a target that kept up
# with its own inflation, exactly as the withdrawals are.
bequeathment_target_real <- bequeathment_pct * pot

# Yield Curve Simulation ----------------------------------------------------
# Fits the PCA/AR(1) model to Yield_Data and simulates it forward over the
# same horizon/n_sims as the equity & bond bootstrap above, so month/sim
# indices line up across the two models. Query with yc$get_curve(month, sim)
# wherever discounting or bond valuation happens below.
yc <- simulate_yield_curves(
  data           = Yield_Data,
  horizon_months = horizon,
  n_sims         = num_sims,
  seed           = 392
)

# Real yield curve + CPI indexation ------------------------------------------
# Real curve(t, path) = Nominal curve(t, path) - Breakeven inflation(t, path).
# Breakeven is its own mean-reverting stochastic process (Functions/
# Real_Yield_Curve.R), NOT a flat subtraction - centered on 4.5%, with
# mean-reversion speed/vol exposed as named, unvalidated placeholder
# parameters. yc (nominal, above) is used exactly as built - not modified.
#
# Reference CPI(t, path) is built from the SAME bootstrapped
# inflation_monthly_ratios used everywhere else in this script -
# Functions/ILB_Indexation.R does not bootstrap or reload CPI itself, it
# only turns this one shared series into absolute index levels, anchored to
# the last actual (non-bootstrapped) CPI Index value already computed above
# (cpi_values). Both real_yc and reference_cpi feed the ILB bond selector's
# repricing at future review dates inside run_dynamic_ladder_simulation().
breakeven_sim <- simulate_breakeven_inflation(horizon_months = horizon, n_sims = num_sims)
real_yc        <- build_real_yield_curve(yc, breakeven_sim)

cpi_anchor    <- cpi_values[length(cpi_values)]
reference_cpi <- reference_cpi_path(inflation_monthly_ratios, cpi_anchor)

# The bootstrapped CPI path is anchored to the last actual CPI print; the ILB
# workbook's index ratios are struck off ILB_REFERENCE_CPI. Every real -> nominal
# conversion in the model divides one by the other, so if a data refresh ever
# moves one without the other the whole run is silently mis-scaled. Stop here
# rather than warn - this multiplies into every deposit, mark and purchase.
assert_cpi_anchor(cpi_anchor, ILB_REFERENCE_CPI)

# Soft check (reports, does not stop): is the flat inflation_rate the model
# ASSUMES consistent with the inflation the bootstrap actually DELIVERS? A gap
# here is a modelling choice rather than an arithmetic error, so it is surfaced
# and left to the reader - but it is surfaced, because the last time the two
# disagreed by 0.8pp it put 86.5% of ladders into overdraft.
check_inflation_assumption(inflation_monthly_ratios, inflation_rate)

# The single real -> nominal bridge, [horizon+1 x n_sims]: row k+1 is the factor
# for elapsed month k, and row 1 is a literal 1 (a real rand at t0 IS a nominal
# rand at t0). The engine builds its own copy of this internally; this one is for
# the reporting/deflation code at the bottom of the script.
nfac_all <- nominal_factor_matrix(reference_cpi, ILB_REFERENCE_CPI)

# Bond ladder (ILB, real terms) ----------------------------------------------
# Finds the cheapest REAL-terms ILB portfolio that cash-flow-matches the
# retiree's desired annual withdrawal (wd) over the ladder period - backward
# dedication, Functions/Bond Selection ILB.R - and uses its total_outlay to
# set the equity/bond/cash split of the total pot. total_cost/cash_at_start/
# total_outlay are directly usable against pot with no conversion: a real
# rand at t0 IS a nominal rand at t0 (see that file's own header) - only
# values at FUTURE dates need translating (Functions/ILB_Repricing.R /
# ILB_Indexation.R, used inside run_dynamic_ladder_simulation() below).
res_ilb <- optimize_ilb_ladder(
  ladder_years           = ladder_length,
  total_pot_value        = pot,
  withdrawal_rate_pct    = wd * 100,
  nominal_interest_rate  = cash_annual_rate,
  inflation_rate         = inflation_rate,
  cash_buffer_months     = cash_buffer_months,
  on_infeasible          = "flag"
)

if (res_ilb$total_outlay > pot) {
  stop(sprintf(
    "Bond ladder cost + opening cash (R%.2f) exceeds the total pot (R%.2f) - reduce the withdrawal rate or ladder length.",
    res_ilb$total_outlay, pot
  ))
}

BPort_Split    <- res_ilb$total_cost / pot
CashPort_Split <- res_ilb$cash_at_start / pot

# --- Bond ladder summary ---------------------------------------------------
cat("\n=======================================================\n")
cat("              BOND LADDER (ILB, REAL TERMS)             \n")
cat("=======================================================\n\n")
print_ilb_ladder(res_ilb)

cat(sprintf("\nEquity starting allocation: R%s (%.2f%% of pot)\n",
            format(round(pot - res_ilb$total_outlay, 2), big.mark = ","),
            (1 - BPort_Split - CashPort_Split) * 100))

# ARVA parameters ----------------------------------------------------------
# All retirees are assumed to be age 60 at t = 0 (start of the simulation).
# The ARVA phase now begins at a DIFFERENT month for each simulation path
# (whenever that path's ladder ends - naturally, via cap, or via a
# never-extended original ladder), so retirement_age/ages_arva/af_vector
# are no longer computed once globally - arva_annuity_factor() is instead
# called per-path, per-year, inside run_dynamic_ladder_simulation().
start_age <- 60
max_age   <- 90   # 30-year max retirement period

# Invariant layer ----------------------------------------------------------
# Three checks, cheapest and loudest first (Functions/Invariants.R):
#
#   1. t0 boundary checks - marking the ladder on the day it was bought must
#      return what was paid for it, and with inflation switched off the engine's
#      nominal deposit schedule must reproduce the selector's own real one.
#      Both stop() on failure, so a corrupted run never starts.
#   2. the golden path - the whole engine, on one deterministic path with
#      decisions off, deflated back to t0 rands, must reproduce the selector's
#      real cash schedule month for month.
#   3. the per-(path, month) audit, accumulated during the run and reported at
#      the end (see `audit` below).
#
# These exist because five separate defects in this model were all the same
# silent mistake - a real rand amount spent as a nominal one - and every one of
# them would have been caught on the first run by checks 1 and 2.
run_boundary_checks(
  res_ilb        = res_ilb,
  bonds          = ilb_fixed,
  valuation_date = ILB_MODEL_START_DATE,
  reference_cpi  = reference_cpi,
  cpi_anchor     = cpi_anchor,
  cpi_t0         = ILB_REFERENCE_CPI
)

boundary_check_golden_path(
  res_ilb          = res_ilb,
  bonds            = ilb_fixed,
  valuation_date   = ILB_MODEL_START_DATE,
  pot              = pot,
  wd               = wd,
  ladder_length    = ladder_length,
  cash_annual_rate = cash_annual_rate,
  inflation_rate   = inflation_rate,
  start_age        = start_age,
  max_age          = max_age,
  yc               = yc,
  real_yc          = real_yc,
  cpi_t0           = ILB_REFERENCE_CPI
)

# Accumulator for the in-run identities. Collected on every path, every month,
# and reported once after the run - a stray path is not allowed to abort a
# 1,000-path job, but it cannot hide either.
audit <- audit_new(enabled = TRUE)

# Dynamic ladder + ARVA simulation -------------------------------------
# Runs the full 360-month horizon for all 1,000 paths in one pass: the
# ladder phase (with annual extend/defend/hold checks per path) and the
# ARVA phase (starting whenever each path's own ladder ends), replacing
# the old fixed-ladder_length Phase 1 / Phase 2 loops above.
sim_run_start_time <- Sys.time()
sim_result <- run_dynamic_ladder_simulation(
  pot                     = pot,
  wd                      = wd,
  ladder_length           = ladder_length,
  max_ladder_years        = max_ladder_years,
  extend_by               = extend_by,
  ilb_bonds               = ilb_fixed,
  valuation_date          = ILB_MODEL_START_DATE,
  equity_monthly_returns  = equity_monthly_returns,
  horizon                 = horizon,
  cash_annual_rate        = cash_annual_rate,
  defend_cut              = defend_cut,
  inflation_rate          = inflation_rate,
  start_age               = start_age,
  max_age                 = max_age,
  res_ilb_initial         = res_ilb,
  yc                      = yc,
  real_yc                 = real_yc,
  reference_cpi           = reference_cpi,
  cpi_t0                  = ILB_REFERENCE_CPI,
  bequest_target_real     = bequeathment_target_real,
  decisions_enabled       = TRUE,
  audit                   = audit
)
sim_run_end_time <- Sys.time()
cat(sprintf("\nDynamic ladder + ARVA simulation runtime (%d paths x %d months): %s\n",
            num_sims, horizon, format(sim_run_end_time - sim_run_start_time)))

EPort_history           <- sim_result$EPort_history
Cash_history            <- sim_result$Cash_history
# TOTAL WEALTH = equity + the cash account. This is the series every headline
# number below is computed on - see the ruin block for why.
Wealth_history          <- sim_result$Wealth_history
Cash_deposit_history    <- sim_result$Cash_deposit_history
Cash_withdrawal_history <- sim_result$Cash_withdrawal_history
ARVA_withdrawal_history <- sim_result$ARVA_withdrawal_history

EPort                   <- sim_result$EPort
Cash                    <- sim_result$Cash
Wealth                  <- sim_result$Wealth

# -------------------------------------------------------------------------
# INVARIANT AUDIT
# -------------------------------------------------------------------------
# Every identity checked on every path, every month, reported once. Read this
# BEFORE reading any result below it: a FAIL line means the distribution that
# follows is not describing the model anyone intended.
audit_ok <- audit_report(audit, "SIMULATION AUDIT - ACCOUNTING IDENTITIES")
if (!audit_ok) {
  warning("The simulation audit FAILED - see the report above. Results below are not trustworthy.")
}

# --- Diagnostics the audit deliberately does NOT treat as failures ----------
# Both of these are known, accepted model behaviours rather than broken
# arithmetic, so they are counted and reported rather than raised as breaches.
#
# 1. The cash account is allowed to go negative: nothing tops it up from equity
#    mid-ladder. The ladder is solved to be exactly solvent under the ASSUMED
#    inflation rate, so any path whose realized inflation runs hotter than that
#    outspends its own buffer.
# 2. An Extend & Harvest decision whose purchase the equity sleeve cannot afford
#    is now skipped rather than driving equity negative. Previously nothing
#    enforced affordability at all.
n_paths_neg_cash <- sum(sim_result$cash_negative_months > 0)
cat(sprintf("\nPaths whose cash account went negative at some point: %d / %d\n",
            n_paths_neg_cash, num_sims))
if (n_paths_neg_cash > 0) {
  cat(sprintf("  worst path spent %d months below zero; deepest balance R%s\n",
              max(sim_result$cash_negative_months),
              format(round(min(sim_result$Cash_history), 2), big.mark = ",")))
}
cat(sprintf("Extend decisions skipped as unaffordable: %d\n", sim_result$n_unaffordable))

# --- What the extensions actually cost ------------------------------------
# Split out because the two legs are funded for different reasons and used to be
# conflated. bond_cost is a genuine increment - more of the anchor, or a new bond.
# The cash top-up is the SHORTFALL between what the pooled account is projected
# to hold on the sub-ladder's start date and what that sub-ladder needs there.
# It used to be the full requirement, recharged every year even when the anchor
# had not moved, which quietly turned equity into idle cash.
cat(sprintf("\nExtensions executed: %d across %d paths (median %d per path)\n",
            sum(sim_result$n_extensions), num_sims, median(sim_result$n_extensions)))
cat(sprintf("  harvested for bonds      : median R%s per path\n",
            format(round(median(sim_result$extension_bond_cost)), big.mark = ",")))
cat(sprintf("  harvested for cash bridge: median R%s per path (%d paths needed none at all)\n",
            format(round(median(sim_result$extension_cash_topup)), big.mark = ","),
            sum(sim_result$extension_cash_topup == 0)))

# --- Did the real income actually hold level? ------------------------------
# The whole point of a real ladder. real_income only ever moves on a Defend cut,
# so the final level relative to the starting one is a direct readout of how
# often each path had to defend. Income_real_history is the ACTUAL monthly
# payment deflated back to t0 rands, which is the number that has to stay flat.
cat(sprintf("\nStarting real income: R%s p.a.\n",
            format(round(wd * pot, 2), big.mark = ",")))
cat(sprintf("Final real income across paths: median R%s (5th pct R%s, 95th pct R%s)\n",
            format(round(median(sim_result$real_income_final), 2), big.mark = ","),
            format(round(quantile(sim_result$real_income_final, 0.05), 2), big.mark = ","),
            format(round(quantile(sim_result$real_income_final, 0.95), 2), big.mark = ",")))
cat(sprintf("Paths that never had to cut income: %d / %d\n",
            sum(sim_result$real_income_final >= wd * pot - 1e-6), num_sims))

# "Locked" replaces the old Redington-era "frozen on infeasible rebalance" -
# a path whose anchor bond matured before it was ever extended far enough to
# replace it (see Functions/Dynamic_Ladder.R's ladder_locked note). Its
# ladder still runs to its own committed end and liquidates into ARVA
# normally - "locked" only means no FURTHER bond purchases happen for it.
cat(sprintf("\nPaths whose ladder locked (anchor matured before being replaced): %d / %d\n",
            sum(sim_result$ladder_locked), num_sims))
cat("Distribution of final ladder length across paths:\n")
print(table(sim_result$ladder_years_final))

# -------------------------------------------------------------------------
# SUMMARY STATISTICS & DISTRIBUTION ANALYSIS
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 1. INCOME DELIVERED - the primary lens
# -------------------------------------------------------------------------
# A decumulation strategy is judged first on the income it pays and how
# reliably it pays it, not on what happens to be left at the end. That is
# doubly true here, because ARVA is a SPEND-DOWN rule: it withdraws EPort/af
# every year and is built to run the portfolio down by max_age. Any change that
# makes the strategy more efficient therefore shows up as a LARGER income and a
# SMALLER bequest - measured against the bequeathment target alone, a genuine
# improvement scores as a deterioration. Both are reported; income leads.
#
# Income_real_history is the payment actually made each month, deflated by that
# path's own realized CPI - today's rands, directly comparable across months,
# across paths, and against the R600,000 the ladder was built to secure.
income_real_m  <- sim_result$Income_real_history                 # [horizon x num_sims]
year_of_month  <- rep(seq_len(horizon %/% 12), each = 12)
income_real_yr <- rowsum(income_real_m, group = year_of_month)   # [30 x num_sims]

total_real_income <- colSums(income_real_m)
worst_year_real   <- apply(income_real_yr, 2, min)
target_income     <- wd * pot

cat("=======================================================\n")
cat("      INCOME DELIVERED (ALL FIGURES IN TODAY'S RANDS)  \n")
cat("=======================================================\n\n")

cat(sprintf("Target real income the ladder was built to secure: R%s p.a.\n\n",
            format(round(target_income), big.mark = ",")))

cat("--- Total real income paid over the 30-year horizon ---\n")
print(format(round(quantile(total_real_income, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))),
             big.mark = ",", scientific = FALSE))
cat(sprintf("Mean: R%s   (a level R%s p.a. for 30 years would be R%s)\n\n",
            format(round(mean(total_real_income)), big.mark = ","),
            format(round(target_income), big.mark = ","),
            format(round(target_income * 30), big.mark = ",")))

cat("--- Annual real income, pooled over every path-year ---\n")
print(format(round(quantile(as.numeric(income_real_yr), probs = c(0.05, 0.25, 0.50, 0.75, 0.95))),
             big.mark = ",", scientific = FALSE))

cat("\n--- The income FLOOR: each path's single worst year ---\n")
print(format(round(quantile(worst_year_real, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))),
             big.mark = ",", scientific = FALSE))
cat(sprintf("Paths whose worst year still cleared the target: %.1f%%\n",
            100 * mean(worst_year_real >= target_income - 1e-6)))
cat(sprintf("Path-years paid below the target income:        %.1f%%\n",
            100 * mean(income_real_yr < target_income - 1e-6)))

# Split by phase, since the two halves of the strategy behave completely
# differently: the ladder pays a level real income by construction, ARVA pays
# EPort/af and therefore tracks the market.
ladder_income <- numeric(num_sims); arva_income <- numeric(num_sims)
for (s in seq_len(num_sims)) {
  le <- sim_result$ladder_end_month[s]
  le <- if (is.na(le)) horizon else le
  ladder_income[s] <- sum(income_real_m[seq_len(le), s])
  arva_income[s]   <- if (le < horizon) sum(income_real_m[(le + 1):horizon, s]) else 0
}
cat(sprintf("\nMedian real income by phase: ladder R%s | ARVA R%s\n",
            format(round(median(ladder_income)), big.mark = ","),
            format(round(median(arva_income)),   big.mark = ",")))
# ARVA is sized on equity PLUS cash and spends the cash it already holds first,
# so the amount actually pulled out of equity is strictly less than the income
# it funded. The gap is the ladder-phase residual being put to work.
arva_income_nom <- colSums(sim_result$Cash_withdrawal_history *
                             (row(sim_result$Cash_withdrawal_history) >
                                matrix(ifelse(is.na(sim_result$ladder_end_month), horizon,
                                              sim_result$ladder_end_month),
                                       nrow = horizon, ncol = num_sims, byrow = TRUE)))
cat(sprintf("ARVA income paid R%s nominal per path (median), of which drawn from equity R%s\n",
            format(round(median(arva_income_nom)), big.mark = ","),
            format(round(median(sim_result$arva_equity_drawn)), big.mark = ",")))
cat(sprintf("  -> funded from cash already held: median R%s (%.1f%% of ARVA income)\n",
            format(round(median(arva_income_nom - sim_result$arva_equity_drawn)), big.mark = ","),
            100 * median((arva_income_nom - sim_result$arva_equity_drawn) /
                           pmax(arva_income_nom, 1))))

# ARVA now amortises down to the bequest rather than to zero, with the real
# ladder income as a floor. How often does that floor bind? A year where it
# binds is a year the retiree kept eating and the estate paid for it - so this
# is the direct read on how much of the bequest shortfall is deliberate policy
# rather than market outcome.
arva_years_total <- sum(!is.na(sim_result$ARVA_withdrawal_history))
cat(sprintf("ARVA years where the income FLOOR bound (bequest deliberately sacrificed): %d of %d (%.1f%%)\n",
            sum(sim_result$arva_floor_years), arva_years_total,
            100 * sum(sim_result$arva_floor_years) / max(arva_years_total, 1)))
cat(sprintf("  paths where it never bound: %d / %d\n",
            sum(sim_result$arva_floor_years == 0), num_sims))

cat("\n=======================================================\n")
cat("   TERMINAL WEALTH SUMMARY (EQUITY + RESIDUAL CASH)    \n")
cat("=======================================================\n\n")

# Basic Five-Number Summary + Mean
cat("--- Standard Summary Statistics (nominal) ---\n")
print(summary(Wealth))

# Detailed Percentile Distribution
cat("\n--- Percentile Distribution (ZAR, nominal) ---\n")
percentiles <- quantile(Wealth, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
print(format(percentiles, big.mark = ",", scientific = FALSE))

# The two sleeves separately, so the split is visible rather than assumed.
#
# The residual used to be dominated by ARVA float interest: the annual draw is
# moved into cash in one lump and paid out at 1/12 a month, so half a year of
# income sits idle earning cash_annual_rate, and that interest simply compounded
# into the closing balance. On one traced path the ARVA deposits and payments
# were exactly equal (R214,465,014 each) and terminal cash of R8,248,175 WAS the
# accumulated interest. That interest is now paid to the retiree in the month it
# is earned (see the ARVA branch in Dynamic_Ladder.R), so what remains here is
# the genuine ladder-phase residual: the opening buffer, Defend over-funding,
# real-rate drift, and surplus the additive extension design never sweeps back.
cat("\n--- Terminal split: equity vs residual cash (nominal medians) ---\n")
cat(sprintf("  equity        : R%s\n", format(round(median(EPort)), big.mark = ",")))
cat(sprintf("  residual cash : R%s\n", format(round(median(Cash)),  big.mark = ",")))
cat(sprintf("  total wealth  : R%s\n", format(round(median(Wealth)), big.mark = ",")))
cat(sprintf("  paths ending with cash < 0: %d / %d\n", sum(Cash < 0), num_sims))

# Ruin & Bequeathment Metrics
# "Ruin" means failing the retiree's own bequeathment target - ending the horizon
# with less than bequeathment_pct of the starting pot IN REAL TERMS - not just
# literal depletion to R0. Complete depletion is reported separately, since it is
# a stricter (and rarer) outcome worth seeing on its own.
#
# nfac_all[horizon + 1, ] is each path's cumulative CPI over the full 360 months
# (row k+1 = elapsed month k), so this is R1,000,000 of today's money expressed
# in each path's own end-of-horizon rands.
bequeathment_target_path <- bequeathment_target_real * nfac_all[horizon + 1, ]

# RUIN IS MEASURED ON TOTAL WEALTH: terminal equity PLUS the residual cash
# balance. It used to be measured on EPort alone, which understated terminal
# wealth systematically rather than randomly: in the ARVA phase the annual draw
# moves money OUT of equity and INTO the cash account, and the annuity factor
# approaches 1 as age approaches max_age, so the last withdrawals move nearly
# everything across.
#
# It also closes a loophole. The cash account is allowed to go negative (nothing
# harvests equity to cover a shortfall - a deliberate modelling choice, counted
# and reported above). On the old metric an overdraft was FREE, because ruin
# never looked at the cash balance. It is now netted off terminal wealth, so a
# path that finances its income by running the account down is scored on it.
#
# The bond ladder is not a third term: by month 360 every path has long since
# liquidated it into equity.
ruin_prob      <- mean(Wealth < bequeathment_target_path)
depletion_prob <- mean(Wealth <= 0)

# Terminal values deflated back to t0 rands - the only basis on which outcomes
# 30 years apart are comparable, and the basis the target is set on.
Wealth_real_terminal <- Wealth / nfac_all[horizon + 1, ]
EPort_real_terminal  <- EPort  / nfac_all[horizon + 1, ]

cat(sprintf("\nBequeathment target: R%s in TODAY'S rands (%.0f%% of the starting pot)\n",
            format(round(bequeathment_target_real, 2), big.mark = ","), bequeathment_pct * 100))
cat(sprintf("  -> per-path nominal target at month %d: median R%s (range R%s - R%s)\n",
            horizon,
            format(round(median(bequeathment_target_path), 2), big.mark = ","),
            format(round(min(bequeathment_target_path), 2),    big.mark = ","),
            format(round(max(bequeathment_target_path), 2),    big.mark = ",")))
cat(sprintf("PROBABILITY OF RUIN (equity + residual cash < real target): %.2f%%\n", ruin_prob * 100))
cat(sprintf("Probability of Complete Depletion (total wealth <= R0): %.2f%%\n", depletion_prob * 100))

# The old equity-only figure, kept for continuity with earlier runs and so the
# size of the measurement effect stays visible rather than being quietly absorbed
# into a headline that moved.
cat(sprintf("  [for reference, the old equity-only measure: %.2f%%]\n",
            100 * mean(EPort < bequeathment_target_path)))

cat("\n--- Terminal wealth in TODAY'S rands (deflated by each path's own CPI) ---\n")
print(format(round(quantile(Wealth_real_terminal,
                            probs = c(0.05, 0.25, 0.50, 0.75, 0.95))),
             big.mark = ",", scientific = FALSE))
cat(sprintf("Mean terminal wealth, real: R%s   (equity only: R%s)\n",
            format(round(mean(Wealth_real_terminal), 2), big.mark = ","),
            format(round(mean(EPort_real_terminal),  2), big.mark = ",")))

# Mean & Standard Deviation
cat(sprintf("Mean terminal wealth, nominal: R%s\n", format(round(mean(Wealth), 2), big.mark = ",")))
cat(sprintf("Standard Deviation: R%s\n", format(round(sd(Wealth), 2), big.mark = ",")))
cat("=======================================================\n")

# -------------------------------------------------------------------------
# 2. PLOTTING ALL 1,000 SIMULATION PATHS
# -------------------------------------------------------------------------
# Time index and quantile trajectory vectors.
#
# PLOTTED SERIES: total LIQUID wealth = equity + the cash account, which is the
# series ruin is now measured on. Note what it excludes MID-RUN: the bond ladder
# itself is not marked monthly (repricing is per-path and expensive - see the
# annual-review note in Dynamic_Ladder.R), so between t0 and a path's own ladder
# end this understates the full portfolio by the ladder's market value. It is
# exact from each path's liquidation onward, and therefore exact at month 360,
# which is where every headline number is taken. The axis label says so.
months <- 0:horizon
p05 <- apply(Wealth_history, 1, quantile, probs = 0.05)
p50 <- apply(Wealth_history, 1, quantile, probs = 0.50)
p95 <- apply(Wealth_history, 1, quantile, probs = 0.95)

# Scale values to Millions for clean plotting
Wealth_mils <- Wealth_history / 1e6

matplot(
  months, Wealth_mils,
  type = "l",
  lty = 1,
  col = rgb(0.2, 0.4, 0.8, alpha = 0.03), # Semi-transparent blue for individual paths
  xlab = "Month",
  ylab = "Equity + Cash (ZAR Millions; excl. unliquidated ladder)",
  main = "Monte Carlo Simulation: 30-Year Liquid Wealth Paths (1,000 Trajectories)",
  ylim = c(0, quantile(Wealth_mils, 0.99) * 1.05)
)

# End of Ladder marker - every path now ends its ladder at a DIFFERENT
# month (extensions/cap/natural maturity all differ by path), so there's no
# single "end of ladder" anymore. The median across all 1,000 paths is
# shown as a representative reference line, clearly labeled as a median,
# not a hard boundary that applies to every path.
median_ladder_end <- median(sim_result$ladder_end_month, na.rm = TRUE)
abline(v = median_ladder_end, col = "darkgreen", lty = 2, lwd = 2)

# Bequeathment target marker - the retiree's own bar for "success" at the end.
# Now path-specific (each path's real target inflated by its own realized CPI),
# so the MEDIAN target is drawn and labelled as such.
abline(h = median(bequeathment_target_path) / 1e6, col = "purple", lty = 3, lwd = 2)

# Overlay Key Risk & Expected Trajectories
lines(months, p05 / 1e6, col = "firebrick", lwd = 2.5) # 5th Percentile
lines(months, p50 / 1e6, col = "black",     lwd = 3.0) # Median (50th Percentile)
lines(months, p95 / 1e6, col = "darkgreen", lwd = 2.5) # 95th Percentile

# Legend
legend(
  "topleft",
  legend = c("95th Percentile", "Median (50th)", "5th Percentile", "Median End of Ladder", "Bequeathment Target (median)"),
  col = c("darkgreen", "black", "firebrick", "darkgreen", "purple"),
  lty = c(1, 1, 1, 2, 3),
  lwd = c(2.5, 3, 2.5, 2, 2),
  bg = "white",
  cex = 0.85
)

# -------------------------------------------------------------------------
# 2b. THE SAME PATHS IN TODAY'S RANDS
# -------------------------------------------------------------------------
# The nominal fan chart above is dominated by inflation: at 5% a year, a flat
# real portfolio still rises 4.3x over 360 months. Deflating each path by its OWN
# realized CPI is what makes the vertical axis mean something the retiree can
# use, and it is the basis the bequeathment target is set on.
Wealth_real_history <- Wealth_history / nfac_all
Wealth_real_mils    <- Wealth_real_history / 1e6

matplot(
  months, Wealth_real_mils,
  type = "l", lty = 1,
  col = rgb(0.2, 0.5, 0.3, alpha = 0.03),
  xlab = "Month",
  ylab = "Equity + Cash (ZAR Millions, today's rands)",
  main = "Liquid Wealth in REAL Terms (1,000 Trajectories)",
  ylim = c(0, quantile(Wealth_real_mils, 0.995) * 1.05)
)
lines(months, apply(Wealth_real_history, 1, quantile, probs = 0.05) / 1e6, col = "firebrick", lwd = 2.5)
lines(months, apply(Wealth_real_history, 1, quantile, probs = 0.50) / 1e6, col = "black",     lwd = 3.0)
lines(months, apply(Wealth_real_history, 1, quantile, probs = 0.95) / 1e6, col = "darkgreen", lwd = 2.5)
abline(h = bequeathment_target_real / 1e6, col = "purple", lty = 3, lwd = 2)
abline(v = median_ladder_end, col = "darkgreen", lty = 2, lwd = 2)
legend(
  "topleft",
  legend = c("95th Percentile", "Median (50th)", "5th Percentile",
             "Median End of Ladder", "Bequeathment Target (real)"),
  col = c("darkgreen", "black", "firebrick", "darkgreen", "purple"),
  lty = c(1, 1, 1, 2, 3), lwd = c(2.5, 3, 2.5, 2, 2),
  bg = "white", cex = 0.85
)

# --- Real income actually paid, across paths -------------------------------
# Income_real_history is the monthly payment deflated to t0 rands, annualised.
# On a path that never defends this is a flat line at wd * pot - which is the
# visual statement of "level real income" the whole strategy is built on.
income_real_annual <- sim_result$Income_real_history * 12
matplot(
  1:horizon, income_real_annual,
  type = "l", lty = 1, col = rgb(0.6, 0.3, 0.1, alpha = 0.03),
  xlab = "Month", ylab = "Income Paid (ZAR p.a., today's rands)",
  main = "Realised Income in REAL Terms (all paths)"
)
lines(1:horizon, apply(income_real_annual, 1, quantile, probs = 0.05), col = "firebrick", lwd = 2)
lines(1:horizon, apply(income_real_annual, 1, median),                 col = "black",     lwd = 2.5)
abline(h = wd * pot, col = "purple", lty = 3, lwd = 2)
legend("bottomleft",
       legend = c("Median", "5th Percentile", "Target real income"),
       col = c("black", "firebrick", "purple"),
       lty = c(1, 1, 3), lwd = c(2.5, 2, 2), bg = "white", cex = 0.85)

sim_index <- 1   # change to inspect a different simulation path throughout the checks below

# This path's REAL ladder end month - varies path to path (extensions, cap,
# or natural maturity all land on different months) - everything below uses
# this, not the static starting ladder_length.
sim_ladder_end_month <- sim_result$ladder_end_month[sim_index]
sim_ladder_years     <- sim_result$ladder_years_final[sim_index]

cat(sprintf("\nSimulation #%d: ladder ran for %.0f years (ended month %d), final ladder length %d, locked (anchor matured before replacement): %s\n",
            sim_index, sim_ladder_end_month / 12, sim_ladder_end_month, sim_ladder_years,
            sim_result$ladder_locked[sim_index]))
cat("Decision history for this path:\n")
print(sim_result$decision_log[sim_result$decision_log$sim == sim_index, -1], row.names = FALSE)

# Same decision history, opened as a proper data table in its own tab/pane
# (RStudio's spreadsheet-style Viewer) instead of a wrapped console print -
# the bonds_bought column especially reads much better here, since it's a
# free-text "code: units; code: units" string that print() word-wraps.
# Guarded: View() needs RStudio, and an unguarded call is what stopped this
# script running headless under Rscript at all.
if (interactive() && exists("View")) {
  View(
    sim_result$decision_log[sim_result$decision_log$sim == sim_index, -1],
    title = sprintf("Decision Log - Sim #%d", sim_index)
  )
}

# -------------------------------------------------------------------------
# 2b. CASH ACCOUNT BALANCE CHECK (single simulation, monthly)
# -------------------------------------------------------------------------
# Confirms the cash buffer + monthly draw-down mechanic keeps the account
# solvent throughout - balance should never dip below zero. Deterministic
# (identical across sims) up to end of ladder; varies by simulation after
# ARVA withdrawals start.
plot(
  0:horizon, Cash_history[, sim_index],
  type = "l", col = "darkcyan", lwd = 2,
  xlab = "Month", ylab = "Cash Account Balance (ZAR)",
  main = sprintf("Cash Account Balance Over Time (Simulation #%d)", sim_index),
  yaxt = "n"
)
# Custom y-axis: plain comma-formatted numbers instead of R's default
# scientific notation (e.g. "6,000,000" instead of "6e+06").
y_ticks <- pretty(Cash_history[, sim_index])
axis(2, at = y_ticks, labels = format(y_ticks, big.mark = ",", scientific = FALSE))
abline(h = 0, col = "red", lty = 2)
abline(v = sim_ladder_end_month, col = "darkgreen", lty = 2, lwd = 2)
legend(
  "topright",
  legend = c("Cash Balance", "Zero", "End of Ladder"),
  col    = c("darkcyan", "red", "darkgreen"),
  lty    = c(1, 2, 2),
  lwd    = c(2, 1, 2),
  bg = "white", cex = 0.85
)

# --- Monthly cash account table (simulation #sim_index) --------------------
# Full month-by-month detail: deposit in, withdrawal out, resulting balance.
# Useful for tracing exactly when coupons/redemptions/ARVA withdrawals land
# and confirming the account never goes negative.
cash_account_table <- data.frame(
  Month      = 1:horizon,
  Year       = ceiling((1:horizon) / 12),
  Phase      = ifelse(1:horizon <= sim_ladder_end_month, "Ladder", "ARVA"),
  Deposit    = Cash_deposit_history[, sim_index],
  Withdrawal = Cash_withdrawal_history[, sim_index],
  Balance    = Cash_history[2:(horizon + 1), sim_index]
)

cat("\n=======================================================\n")
cat(sprintf("   CASH ACCOUNT - MONTHLY DETAIL, SIMULATION #%d       \n", sim_index))
cat("=======================================================\n\n")
print(cash_account_table, row.names = FALSE)

cat(sprintf("\nMinimum balance reached: R%s (month %d)\n",
            format(round(min(cash_account_table$Balance), 2), big.mark = ","),
            cash_account_table$Month[which.min(cash_account_table$Balance)]))

# -------------------------------------------------------------------------
# 3. CASH FLOW TIMELINE CHECK (single simulation)
# -------------------------------------------------------------------------
# Lays out every cash flow, in order, for ONE simulation path - built
# entirely from that path's own REAL results (Cash_deposit_history and
# sim_result$bond_sale_amount), not from the original static 7-year
# assumption. Each path's ladder may have extended, defended, or matured
# naturally at a completely different year, so there is no single fixed
# "ladder_length" or "n_arva_years" that applies across paths - using those
# static values here (as an earlier draft of this script did) produced a
# stale, mislabeled timeline that could visually overlap/collide with the
# real one once a path's actual ladder end diverged from year 7.
#
# Cash_deposit_history already records, per month, per path: bond ladder
# coupon/redemption deposits during the ladder phase, and the annual ARVA
# withdrawal during the ARVA phase (both land in the cash account) - so it's
# a complete, accurate record of every CASH inflow for this path. The bond
# ladder LIQUIDATION at ladder-end is a separate, one-off event that goes
# straight into the EQUITY portfolio (not the cash account) - that's
# sim_result$bond_sale_amount[sim_index], now returned from
# run_dynamic_ladder_simulation() specifically so it can be shown here
# instead of being invisible (folded silently into EPort).
deposits_this_sim <- Cash_deposit_history[, sim_index]
timeline_months <- 1:horizon
timeline_df <- data.frame(
  year      = ceiling(timeline_months / 12),
  cash_flow = deposits_this_sim,
  source    = ifelse(timeline_months <= sim_ladder_end_month, "Cash Account (Ladder)", "Cash Account (ARVA)")
)
timeline_df <- timeline_df[timeline_df$cash_flow > 0, ]  # only months with an actual deposit/withdrawal

timeline <- aggregate(cash_flow ~ year + source, data = timeline_df, sum)
timeline$type <- ifelse(timeline$source == "Cash Account (Ladder)", "Bond Coupon/Redemption", "ARVA Withdrawal")

# Insert the one-off bond ladder liquidation - this path's REAL value at its
# REAL exit month, not a static day-0 estimate
bond_sale_row <- data.frame(
  year      = sim_ladder_end_month / 12,
  source    = "Equity Portfolio (one-off)",
  cash_flow = sim_result$bond_sale_amount[sim_index],
  type      = "Bond Ladder Liquidation"
)
timeline <- rbind(timeline[, c("year", "source", "type", "cash_flow")], bond_sale_row)
timeline <- timeline[order(timeline$year), ]

# --- Equity portfolio value overlay -----------------------------------------
# This path's own equity portfolio value at each year-end (month 0, 12, 24,
# ...) added alongside its cash flow events, so you can see how EPort itself
# evolves through both the ladder and ARVA phases on the same year axis.
# Kept as a SEPARATE row type ("Equity Portfolio Value") rather than another
# cash_flow entry to add - it's a balance/level at a point in time, not a
# flow, and its scale is usually far larger than any single coupon,
# redemption, or ARVA withdrawal.
equity_year_end_months <- seq(1, horizon + 1, by = 12)   # index into EPort_history: month 0, 12, 24, ...
equity_timeline <- data.frame(
  year          = (equity_year_end_months - 1) / 12,
  equity_value  = EPort_history[equity_year_end_months, sim_index]
)

# The bar chart's y-scale is set from cash FLOWS only (coupons/redemptions/
# ARVA/liquidation) - saved off here BEFORE the equity rows are merged in,
# since equity's much larger scale would otherwise swamp the bars down to
# invisible slivers if it set the axis range.
bar_timeline <- timeline

timeline <- rbind(
  timeline,
  data.frame(
    year      = equity_timeline$year,
    source    = "Equity Portfolio",
    type      = "Equity Portfolio Value (period-end)",
    cash_flow = equity_timeline$equity_value
  )
)
timeline <- timeline[order(timeline$year), ]

cat("\n=======================================================\n")
cat(sprintf("   CASH FLOW TIMELINE - SIMULATION #%d                 \n", sim_index))
cat("=======================================================\n\n")
print(timeline, row.names = FALSE)

# --- Timeline plot ---------------------------------------------------------
plot_cols <- c(
  "Bond Coupon/Redemption"   = "steelblue",
  "Bond Ladder Liquidation"  = "purple",
  "ARVA Withdrawal"          = "darkorange"
)

plot(
  bar_timeline$year, bar_timeline$cash_flow,
  type = "n",
  xlab = "Year", ylab = "Cash Flow (ZAR)",
  main = sprintf("Bond Ladder & ARVA Cash Flow Timeline (Simulation #%d)", sim_index),
  ylim = c(0, max(bar_timeline$cash_flow, na.rm = TRUE) * 1.1)
)
abline(v = sim_ladder_end_month / 12, col = "darkgreen", lty = 2, lwd = 2)

for (tp in names(plot_cols)) {
  sub <- bar_timeline[bar_timeline$type == tp, ]
  if (nrow(sub) > 0) segments(sub$year, 0, sub$year, sub$cash_flow, col = plot_cols[tp], lwd = 6)
}

legend(
  "topright",
  legend = c(names(plot_cols), "End of This Path's Ladder"),
  col    = c(plot_cols, "darkgreen"),
  lwd    = c(6, 6, 6, 2),
  lty    = c(1, 1, 1, 2),
  bg = "white", cex = 0.85
)

# -------------------------------------------------------------------------
# TOTAL RUNTIME
# -------------------------------------------------------------------------
script_end_time <- Sys.time()
cat("\n=======================================================\n")
cat(sprintf("Total script runtime: %s\n", format(script_end_time - script_start_time)))
cat("=======================================================\n")

# -------------------------------------------------------------------------
# 4. YIELD CURVE SIMULATION DIAGNOSTIC PLOTS
# -------------------------------------------------------------------------
# `yc` was fit earlier (Yield Curve Simulation section, above the bond ladder)
# over the same horizon/n_sims as the equity & bond bootstrap, so month/sim
# indices here line up with everything else in the script.

# --- 4a. Historical mean curve the PCA model was fit to --------------------
plot(
  yc$tenors_months, yc$mean_curve,
  type = "l", lwd = 2, col = "black",
  xlab = "Tenor (months)", ylab = "Yield (%)",
  main = "Historical Mean SA Nominal Yield Curve (PCA fit)"
)

# --- 4b. Simulated term structure at several horizons, one path ------------
# Uses its own index (not the ladder simulation's `sim_index`, defined later
# in the script) so this section can be run on its own - `yc` doesn't depend
# on the bond ladder / dynamic simulation at all.
yc_sim_index <- 1
plot_months <- c(1, 60, 120, 240, 360)
plot_months <- plot_months[plot_months <= horizon]
curve_cols <- c("black", "steelblue", "darkorange", "firebrick", "purple")[seq_along(plot_months)]
curves_at_months <- sapply(plot_months, function(m) yc$get_curve(month = m, sim = yc_sim_index))

matplot(
  yc$tenors_months, curves_at_months,
  type = "l", lty = 1, lwd = 2, col = curve_cols,
  xlab = "Tenor (months)", ylab = "Yield (%)",
  main = sprintf("Simulated Yield Curve Term Structure Over Time (Sim #%d)", yc_sim_index)
)
legend(
  "bottomright",
  legend = paste0("Month ", plot_months),
  col = curve_cols, lwd = 2, bg = "white", cex = 0.85
)

# --- 4c. Fan chart: one tenor's simulated path across all sims -------------
tenor_to_plot <- "120M"   # 10Y point
tenor_idx <- which(names(yc$mean_curve) == tenor_to_plot)
tenor_paths <- sapply(1:num_sims, function(s) {
  yc$mean_curve[tenor_idx] +
    yc$pc_sim[, , s] %*% yc$loadings_k[tenor_idx, ] +
    yc$resid_sim[, tenor_idx, s]
})

p05_yield <- apply(tenor_paths, 1, quantile, probs = 0.05)
p50_yield <- apply(tenor_paths, 1, quantile, probs = 0.50)
p95_yield <- apply(tenor_paths, 1, quantile, probs = 0.95)

matplot(
  1:horizon, tenor_paths,
  type = "l", lty = 1,
  col = rgb(0.2, 0.4, 0.8, alpha = 0.03),
  xlab = "Month", ylab = sprintf("%s Yield (%%)", tenor_to_plot),
  main = sprintf("Simulated %s Yield Paths (%d Simulations)", tenor_to_plot, num_sims)
)
lines(1:horizon, p05_yield, col = "firebrick", lwd = 2)
lines(1:horizon, p50_yield, col = "black",     lwd = 2.5)
lines(1:horizon, p95_yield, col = "darkgreen", lwd = 2)
legend(
  "topright",
  legend = c("95th Percentile", "Median", "5th Percentile"),
  col = c("darkgreen", "black", "firebrick"),
  lwd = c(2, 2.5, 2), bg = "white", cex = 0.85
)

