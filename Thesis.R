library(readxl)
library(dplyr)
library(here)
source("functions/moving_block_bootstrap.R")
source("functions/Reddington.R")
source("functions/ARVA.R")
source("functions/Decision_Matrix.R")
source("functions/Dynamic_Ladder.R")

# Timing --------------------------------------------------------------------
# Overall script clock - started as early as possible (right after the
# library/source calls, before any data loading) so the total includes
# everything: data import, bootstrapping, the dynamic ladder simulation, and
# all the summary/plotting code below. Printed at the very end of the script.
script_start_time <- Sys.time()

# Data --------------------------------------------------------------------

# --- 1. Import data ---
Bond_Equity_Data <- read_excel(here("data", "Bond & Equity Data.xlsx"), sheet = "LT_DATA", skip = 1)
CPI_Data <- read_excel(here("data", "CPI_Index_Long_Format.xlsx"))

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
wd <- 0.05                # Bond ladder withdrawal rate the retiree wants to secure each year
ladder_length <- 10        # STARTING ladder length - can extend annually up to max_ladder_years
max_ladder_years <- 15    # Hard cap - reaching it forces liquidation into equity, unconditionally
extend_by <- 1            # Years added per Extend & Harvest decision (kept as a parameter, per request)
defend_cut <- 0.05        # Withdrawal cut applied on a Defend year (passed through to decision_matrix())
inflation_rate <- 0.05    # Flat EXPECTED rate - still used for compute_funded_ratio()'s forward
                           # liability projection and decision_matrix()'s annual raise (both are
                           # forward-looking, so can't discount against inflation that hasn't
                           # happened yet). The actual monthly cash withdrawal is now indexed to
                           # the REALIZED bootstrapped inflation_monthly_ratios path instead (see
                           # run_dynamic_ladder_simulation() call below), not this flat number.
bequeathment_pct <- 0.10  # Retiree-set: fraction of the starting pot they want left over at the end of the horizon
horizon <- 360
num_sims <- ncol(equity_monthly_returns)

# The portfolio is a "success" for this retiree if it ends the horizon with
# at least this much left, rather than merely surviving above R0.
bequeathment_target <- bequeathment_pct * pot

# Bond ladder (Redington immunization) --------------------------------------
# Finds the cheapest bond portfolio that immunizes the retiree's desired
# annual withdrawal (wd) over the ladder period, and uses its cost to set
# the equity/bond split of the total pot.
res_redington <- optimize_redington_immunization(
  total_pot_value     = pot,
  withdrawal_rate_pct = wd * 100,
  ladder_years        = ladder_length,
  convexity_buffer    = 0.5
)

if (res_redington$total_cost > pot) {
  stop(sprintf(
    "Bond ladder cost (R%.2f) exceeds the total pot (R%.2f) - reduce the withdrawal rate or ladder length.",
    res_redington$total_cost, pot
  ))
}

BPort_Split <- res_redington$total_cost / pot

cat(sprintf("\nBond ladder cost: R%s (%.2f%% of pot)\n",
            format(round(res_redington$total_cost, 2), big.mark = ","),
            BPort_Split * 100))

# --- Bond ladder summary ---------------------------------------------------
cat("\n=======================================================\n")
cat("            BOND LADDER (REDINGTON) SUMMARY            \n")
cat("=======================================================\n\n")

cat("--- Actuarial Immunization Verification ---\n")
print(res_redington$actuarial_summary, row.names = FALSE)

cat("\n--- Optimal Bond Allocation ---\n")
print(res_redington$portfolio_allocation, row.names = FALSE)
cat(sprintf("\nTotal Bond Ladder Cost: R%s\n",
            format(round(res_redington$total_cost, 2), big.mark = ",")))
cat("=======================================================\n")

# Bond cash flow schedule (for the initial ladder only - sizes the day-0
# cash buffer below). Once the dynamic simulation runs, each path may
# rebalance many times, so this initial schedule is NOT what actually
# happens for most paths - it's only the starting point. Each path's real
# ladder-end liquidation value is computed inside run_dynamic_ladder_simulation()
# at its own actual exit month, and returned as sim_result$bond_sale_amount.
bond_cf <- bond_cashflow_schedule(res_redington$portfolio_allocation, bonds_fixed, valuation_date)

# Ladder cash buffer ----------------------------------------------------------
# Bond coupons arrive semi-annually and redemption arrives once, at maturity -
# none of it lines up with the retiree's monthly spending. A cash buffer
# bridges the gap: it's sized so that a cash account receiving the ladder's
# coupon/redemption deposits and paying out annual_withdrawal/12 every month
# never goes negative across the whole ladder period.
cash_annual_rate <- 0.05   # effective annual rate, compounded monthly (see below)
cash_buffer <- size_ladder_cash_buffer(
  bond_cf           = bond_cf,
  ladder_years      = ladder_length,
  annual_withdrawal = wd * pot,
  cash_annual_rate  = cash_annual_rate,
  inflation_rate    = inflation_rate
)
cash_monthly_rate <- cash_buffer$cash_monthly_rate
C0                <- cash_buffer$C0

cat(sprintf("\nCash buffer required: R%s (earning %.2f%% p.a., compounded monthly => %.4f%%/month)\n",
            format(round(C0, 2), big.mark = ","),
            cash_annual_rate * 100, cash_monthly_rate * 100))

CashPort_Split <- C0 / pot

if (res_redington$total_cost + C0 > pot) {
  stop(sprintf(
    "Bond ladder cost + cash buffer (R%.2f) exceeds the total pot (R%.2f) - reduce the withdrawal rate or ladder length.",
    res_redington$total_cost + C0, pot
  ))
}

cat(sprintf("Equity starting allocation: R%s (%.2f%% of pot)\n",
            format(round(pot - res_redington$total_cost - C0, 2), big.mark = ","),
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
  bonds_fixed             = bonds_fixed,
  valuation_date          = valuation_date,
  convexity_buffer        = 0.5,
  equity_monthly_returns  = equity_monthly_returns,
  horizon                 = horizon,
  cash_annual_rate        = cash_annual_rate,
  defend_cut              = defend_cut,
  inflation_rate          = inflation_rate,
  start_age               = start_age,
  max_age                 = max_age,
  res_redington_initial   = res_redington,
  C0_initial              = C0,
  bond_cf_initial         = bond_cf,
  inflation_monthly_ratios = inflation_monthly_ratios
)
sim_run_end_time <- Sys.time()
cat(sprintf("\nDynamic ladder + ARVA simulation runtime (%d paths x %d months): %s\n",
            num_sims, horizon, format(sim_run_end_time - sim_run_start_time)))

EPort_history           <- sim_result$EPort_history
Cash_history            <- sim_result$Cash_history
Cash_deposit_history    <- sim_result$Cash_deposit_history
Cash_withdrawal_history <- sim_result$Cash_withdrawal_history
ARVA_withdrawal_history <- sim_result$ARVA_withdrawal_history
EPort                   <- sim_result$EPort

cat(sprintf("\nPaths that hit an infeasible rebalance and were frozen: %d / %d\n",
            sum(sim_result$infeasible_flag), num_sims))
cat("Distribution of final ladder length across paths:\n")
print(table(sim_result$ladder_years_final))

# -------------------------------------------------------------------------
# 1. SUMMARY STATISTICS & DISTRIBUTION ANALYSIS
# -------------------------------------------------------------------------

cat("=======================================================\n")
cat("      TERMINAL EQUITY PORTFOLIO (EPort) SUMMARY        \n")
cat("=======================================================\n\n")

# Basic Five-Number Summary + Mean
cat("--- Standard Summary Statistics ---\n")
print(summary(EPort))

# Detailed Percentile Distribution
cat("\n--- Percentile Distribution (ZAR) ---\n")
percentiles <- quantile(EPort, probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
print(format(percentiles, big.mark = ",", scientific = FALSE))

# Ruin & Bequeathment Metrics
# "Ruin" here means failing the retiree's own bequeathment target - ending
# the horizon with less than bequeathment_pct of the starting pot - not
# just literal depletion to R0. Complete depletion is also reported
# separately, since it's a stricter (and rarer) outcome worth seeing on its own.
ruin_prob <- mean(EPort < bequeathment_target)
depletion_prob <- mean(EPort == 0)

cat(sprintf("\nBequeathment target: R%s (%.0f%% of starting pot)\n",
            format(round(bequeathment_target, 2), big.mark = ","), bequeathment_pct * 100))
cat(sprintf("Probability of Ruin (Terminal Value < Bequeathment Target): %.2f%%\n", ruin_prob * 100))
cat(sprintf("Probability of Complete Depletion (Terminal Value = R0): %.2f%%\n", depletion_prob * 100))

# Mean & Standard Deviation
cat(sprintf("Mean Terminal Value: R%s\n", format(round(mean(EPort), 2), big.mark = ",")))
cat(sprintf("Standard Deviation: R%s\n", format(round(sd(EPort), 2), big.mark = ",")))
cat("=======================================================\n")

# -------------------------------------------------------------------------
# 2. PLOTTING ALL 1,000 SIMULATION PATHS
# -------------------------------------------------------------------------

# Time index and quantile trajectory vectors
months <- 0:horizon
p05 <- apply(EPort_history, 1, quantile, probs = 0.05)
p50 <- apply(EPort_history, 1, quantile, probs = 0.50)
p95 <- apply(EPort_history, 1, quantile, probs = 0.95)

# Scale values to Millions for clean plotting
EPort_mils <- EPort_history / 1e6

matplot(
  months, EPort_mils,
  type = "l",
  lty = 1,
  col = rgb(0.2, 0.4, 0.8, alpha = 0.03), # Semi-transparent blue for individual paths
  xlab = "Month",
  ylab = "Equity Portfolio Value (ZAR Millions)",
  main = "Monte Carlo Simulation: 30-Year Equity Portfolio Paths (1,000 Trajectories)",
  ylim = c(0, max(EPort_mils) * 0.35)
)

# End of Ladder marker - every path now ends its ladder at a DIFFERENT
# month (extensions/cap/natural maturity all differ by path), so there's no
# single "end of ladder" anymore. The median across all 1,000 paths is
# shown as a representative reference line, clearly labeled as a median,
# not a hard boundary that applies to every path.
median_ladder_end <- median(sim_result$ladder_end_month, na.rm = TRUE)
abline(v = median_ladder_end, col = "darkgreen", lty = 2, lwd = 2)

# Bequeathment target marker - the retiree's own bar for "success" at the end
abline(h = bequeathment_target / 1e6, col = "purple", lty = 3, lwd = 2)

# Overlay Key Risk & Expected Trajectories
lines(months, p05 / 1e6, col = "firebrick", lwd = 2.5) # 5th Percentile
lines(months, p50 / 1e6, col = "black",     lwd = 3.0) # Median (50th Percentile)
lines(months, p95 / 1e6, col = "darkgreen", lwd = 2.5) # 95th Percentile

# Legend
legend(
  "topleft",
  legend = c("95th Percentile", "Median (50th)", "5th Percentile", "Median End of Ladder", "Bequeathment Target"),
  col = c("darkgreen", "black", "firebrick", "darkgreen", "purple"),
  lty = c(1, 1, 1, 2, 3),
  lwd = c(2.5, 3, 2.5, 2, 2),
  bg = "white",
  cex = 0.85
)

test = as.matrix(EPort)
mean(test)
mean(equity_monthly_returns)

sim_index <- 1   # change to inspect a different simulation path throughout the checks below

# This path's REAL ladder end month - varies path to path (extensions, cap,
# or natural maturity all land on different months) - everything below uses
# this, not the static starting ladder_length.
sim_ladder_end_month <- sim_result$ladder_end_month[sim_index]
sim_ladder_years     <- sim_result$ladder_years_final[sim_index]

cat(sprintf("\nSimulation #%d: ladder ran for %.0f years (ended month %d), final ladder length %d, duration-matched throughout: %s\n",
            sim_index, sim_ladder_end_month / 12, sim_ladder_end_month, sim_ladder_years,
            sim_result$duration_matched_flag[sim_index]))
cat("Decision history for this path:\n")
print(sim_result$decision_log[sim_result$decision_log$sim == sim_index, -1], row.names = FALSE)

# Same decision history, opened as a proper data table in its own tab/pane
# (RStudio's spreadsheet-style Viewer) instead of a wrapped console print -
# the bonds_bought column especially reads much better here, since it's a
# free-text "code: units; code: units" string that print() word-wraps.
View(
  sim_result$decision_log[sim_result$decision_log$sim == sim_index, -1],
  title = sprintf("Decision Log - Sim #%d", sim_index)
)

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

