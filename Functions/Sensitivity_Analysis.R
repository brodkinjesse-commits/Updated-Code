###############################################################################
# SENSITIVITY ANALYSIS - Probability of Ruin vs. Strategy Parameters
###############################################################################
# Companion script to Thesis.R. Produces sensitivity graphs of Probability of
# Ruin (P(Terminal Equity < Bequeathment Target)) against:
#
#   1. Withdrawal rate                      (ARVA vs. fixed/inflation-adjusted
#                                             decumulation, multiple ladder
#                                             length curves)
#   2. Ladder length                        (ARVA vs. fixed/inflation-adjusted
#                                             decumulation, multiple wd curves)
#   3. Bond portfolio cost (in general)     (multiple wd curves)
#   4. Decision matrix used vs. not used    (full dynamic simulation)
#   5. Bequeathment target                  (multiple wd curves)
#
# IMPORTANT SIMPLIFICATION (per instruction) - graphs 1, 2, 3 and 5 IGNORE the
# actual bond ladder mechanics (coupons, redemptions, extend/defend, cash
# buffer). The bond ladder is used ONLY to calculate a once-off bond
# portfolio cost via optimize_redington_immunization(), which sets how much
# of the pot goes into equity at t = 0 (equity0 = pot - bond_cost). That
# equity slug is then:
#   (a) grown, untouched, through the bootstrapped equity returns for the
#       length of the ladder period, then
#   (b) decumulated monthly from the end of the ladder to the end of the
#       360-month horizon, using either ARVA or a flat withdrawal rate that
#       escalates with inflation each year.
# No cash buffer, coupons, or decision-matrix extend/defend logic are
# simulated for these four graphs - that is graph 4's job, which runs the
# REAL run_dynamic_ladder_simulation() (via a small toggle wrapper) so the
# decision matrix on/off comparison is apples-to-apples with Thesis.R.
#
# ASSUMPTIONS I've made that you may want to change (search these tags):
#   [ASSUMPTION: BASE CASE]      - fixed values used whenever a parameter is
#                                   not the one being swept
#   [ASSUMPTION: CURVE VALUES]   - which values of the "other" variable get
#                                   their own curve
#   [ASSUMPTION: RANGE]          - sweep range / step size for each x-axis
#   [ASSUMPTION: DECUMULATION]   - which decumulation method a given sweep
#                                   uses (ARVA-only unless you asked for both)
#
# NOTE ON ARVA.R: this script sources functions/ARVA.R exactly as Thesis.R
# does. That file hard-codes an absolute path to the mortality table xlsx at
# its top - if that path doesn't resolve on this machine, fix it in ARVA.R
# (or point it at data/SAIML98_SAIFL98_Mortality_Table.xlsx, matching the
# relative-path convention used for the other data files) before running
# this script.
###############################################################################

library(readxl)
library(dplyr)
source("functions/moving_block_bootstrap.R")
source("functions/Reddington.R")
source("functions/ARVA.R")
source("functions/Decision_Matrix.R")
source("functions/Dynamic_Ladder.R")

script_start_time <- Sys.time()

save_plots   <- TRUE                       # TRUE => also write a PNG per graph
plot_dir     <- "sensitivity_plots"
if (save_plots && !dir.exists(plot_dir)) dir.create(plot_dir)

# =============================================================================
# 1. DATA (identical to Thesis.R)
# =============================================================================

Bond_Equity_Data <- read_excel("data/Bond & Equity Data.xlsx", sheet = "LT_DATA", skip = 1)
colnames(Bond_Equity_Data)[1] <- "Date"
colnames(Bond_Equity_Data)   <- make.names(colnames(Bond_Equity_Data))
Bond_Equity_Data$Date        <- as.Date(Bond_Equity_Data$Date)

Bond_Equity_Data <- na.omit(Bond_Equity_Data[-1:-420, 1:3])

prices        <- Bond_Equity_Data[, c("SA.Equity.ZAR", "SA.Bonds.ZAR")]
temp_returns  <- prices[-1, ] / prices[-nrow(prices), ]
BE_Returns    <- data.frame(Date = Bond_Equity_Data[-1, 1], temp_returns)
BE_Returns_clean <- BE_Returns[complete.cases(BE_Returns), ]
returns_matrix   <- as.matrix(BE_Returns_clean[, c("SA.Equity.ZAR", "SA.Bonds.ZAR")])

# =============================================================================
# 2. BOOTSTRAP - run ONCE, fixed seed, reused across every sweep below so
#    every curve on every graph is compared against the exact same set of
#    1,000 simulated equity paths (only the strategy parameters change).
# =============================================================================

set.seed(392)
horizon  <- 360
num_sims <- 1000
sim_paths <- moving_block_bootstrap(returns_matrix, block_size = 6, horizon = horizon, n_sims = num_sims)

equity_monthly_returns <- sim_paths[, "SA.Equity.ZAR", ]   # [horizon x num_sims]

# =============================================================================
# 3. BASE CASE PARAMETERS  [ASSUMPTION: BASE CASE]
#    Matches Thesis.R's own defaults. Whenever a graph below sweeps one
#    parameter, every other parameter is held at these values.
# =============================================================================

pot                 <- 10000000
BASE_WD             <- 0.08
BASE_LADDER_YEARS   <- 6
max_ladder_years    <- 15
extend_by           <- 1
defend_cut          <- 0.05
inflation_rate      <- 0.05
BASE_BEQUEATH_PCT   <- 0.10
cash_annual_rate    <- 0.05
convexity_buffer    <- 0.5
start_age           <- 60
max_age             <- 90

bequeathment_target <- function(bq_pct) bq_pct * pot

# =============================================================================
# 4. CORE HELPERS
# =============================================================================

# ---- 4a. Bond portfolio cost via Redington, with graceful infeasibility ----
get_bond_cost <- function(pot, wd, ladder_years, convexity_buffer = 0.5) {
  res <- tryCatch(
    optimize_redington_immunization(
      total_pot_value     = pot,
      withdrawal_rate_pct = wd * 100,
      ladder_years        = ladder_years,
      convexity_buffer    = convexity_buffer,
      on_infeasible        = "flag"
    ),
    error = function(e) list(feasible = FALSE, message = conditionMessage(e))
  )
  if (!isTRUE(res$feasible) || res$total_cost > pot) {
    return(list(feasible = FALSE, total_cost = NA_real_))
  }
  list(feasible = TRUE, total_cost = res$total_cost)
}

# ---- 4b. Grow equity through the (ignored) ladder period, then decumulate --
# decumulation:
#   "ARVA"      -> age-based ARVA drawdown (arva_annuity_factor), same
#                  mechanic as the ARVA phase in run_dynamic_ladder_simulation()
#   "fixed_wd"  -> flat wd% of the ORIGINAL pot, escalated by inflation_rate
#                  every year post-ladder (this is the "no ARVA" comparison
#                  you asked for)
# Returns a length-num_sims vector of terminal (month = horizon) equity values.
grow_and_decumulate <- function(equity0, ladder_years, wd, decumulation,
                                inflation_rate, start_age, max_age, horizon,
                                equity_monthly_returns, pot) {

  n_sims        <- ncol(equity_monthly_returns)
  ladder_months <- round(ladder_years * 12)

  EPort <- if (ladder_months > 0) {
    equity0 * apply(equity_monthly_returns[1:ladder_months, , drop = FALSE], 2, prod)
  } else {
    rep(equity0, n_sims)
  }

  remaining_months <- horizon - ladder_months
  if (remaining_months <= 0) return(EPort)

  current_w        <- wd * pot                 # only used for "fixed_wd"
  n_years_remaining <- ceiling(remaining_months / 12)
  month_cursor      <- ladder_months

  for (yr in 1:n_years_remaining) {
    months_this_year <- min(12, horizon - month_cursor)

    if (decumulation == "ARVA") {
      age_now <- min(start_age + ladder_years + (yr - 1), max_age)
      af <- arva_annuity_factor(age_now, max_age = max_age)
      w  <- if (af > 0) pmin(EPort / af, EPort) else EPort
    } else {
      w <- rep(pmin(current_w, max(current_w, 0)), n_sims)  # flat amount, same for every sim
      w <- pmin(w, EPort)
      current_w <- current_w * (1 + inflation_rate)
    }

    EPort <- pmax(EPort - w, 0)

    if (months_this_year > 0) {
      gf <- apply(
        equity_monthly_returns[(month_cursor + 1):(month_cursor + months_this_year), , drop = FALSE],
        2, prod
      )
      EPort <- EPort * gf
    }
    month_cursor <- month_cursor + months_this_year
  }

  EPort
}

# ---- 4c. Plot helper: y = swept variable, x = P(Ruin), multiple curves ----
plot_ruin_sensitivity <- function(curve_list, xlab, ylab, main, filename = NULL,
                                  legend_pos = "topright") {
  # curve_list: named list of data.frames, each with columns y and ruin_prob
  cols <- c("steelblue", "firebrick", "darkgreen", "purple", "darkorange", "black")
  ltys <- c(1, 2, 3, 4, 5, 6)

  x_all <- unlist(lapply(curve_list, function(d) d$ruin_prob)) * 100
  y_all <- unlist(lapply(curve_list, function(d) d$y))

  draw <- function() {
    plot(NA, xlim = range(x_all, na.rm = TRUE), ylim = range(y_all, na.rm = TRUE),
        xlab = xlab, ylab = ylab, main = main)
    grid(col = "grey85")
    for (i in seq_along(curve_list)) {
      d <- curve_list[[i]][order(curve_list[[i]]$ruin_prob), ]
      lines(d$ruin_prob * 100, d$y, col = cols[((i - 1) %% length(cols)) + 1],
           lty = ltys[((i - 1) %% length(ltys)) + 1], lwd = 2.5)
    }
    legend(legend_pos, legend = names(curve_list),
          col = cols[seq_along(curve_list)], lty = ltys[seq_along(curve_list)],
          lwd = 2.5, bg = "white", cex = 0.8)
  }

  draw()
  if (save_plots && !is.null(filename)) {
    png(file.path(plot_dir, filename), width = 1000, height = 700, res = 120)
    draw()
    dev.off()
  }
}

# =============================================================================
# 5. GRAPH 1 - WITHDRAWAL RATE  [x = wd 2%-10% by 0.1%, axis ticks every 1%]
#    Curves: ladder length in {5, 10, 15}   [ASSUMPTION: CURVE VALUES]
#    Both ARVA and fixed/inflation-adjusted decumulation are computed and
#    shown, distinguished by line type.     [ASSUMPTION: DECUMULATION]
# =============================================================================

cat("\n--- Graph 1: Withdrawal rate sweep ---\n")

wd_seq          <- seq(0.02, 0.10, by = 0.001)      # [ASSUMPTION: RANGE]
ladder_curves_1 <- c(5, 10, 15)                     # [ASSUMPTION: CURVE VALUES]

g1_curves <- list()
for (LL in ladder_curves_1) {
  res_arva  <- data.frame(y = numeric(0), ruin_prob = numeric(0))
  res_fixed <- data.frame(y = numeric(0), ruin_prob = numeric(0))

  for (wdv in wd_seq) {
    bc <- get_bond_cost(pot, wdv, LL, convexity_buffer)
    if (!bc$feasible) next
    equity0 <- pot - bc$total_cost

    term_arva  <- grow_and_decumulate(equity0, LL, wdv, "ARVA",     inflation_rate,
                                      start_age, max_age, horizon, equity_monthly_returns, pot)
    term_fixed <- grow_and_decumulate(equity0, LL, wdv, "fixed_wd", inflation_rate,
                                      start_age, max_age, horizon, equity_monthly_returns, pot)

    bq_tgt <- bequeathment_target(BASE_BEQUEATH_PCT)
    res_arva  <- rbind(res_arva,  data.frame(y = wdv * 100, ruin_prob = mean(term_arva  < bq_tgt)))
    res_fixed <- rbind(res_fixed, data.frame(y = wdv * 100, ruin_prob = mean(term_fixed < bq_tgt)))
  }

  g1_curves[[sprintf("Ladder %dy - ARVA", LL)]]      <- res_arva
  g1_curves[[sprintf("Ladder %dy - Fixed WD", LL)]]  <- res_fixed
  cat(sprintf("  Ladder length %d done\n", LL))
}

plot_ruin_sensitivity(
  g1_curves,
  xlab = "Probability of Ruin (%)",
  ylab = "Withdrawal Rate (%)",
  main = "Sensitivity: Withdrawal Rate vs. Probability of Ruin",
  filename = "01_withdrawal_rate.png"
)

# =============================================================================
# 6. GRAPH 2 - LADDER LENGTH  [x = ladder length 5-15 by 1 year]
#    Curves: withdrawal rate in {4%, 6%, 8%}   [ASSUMPTION: CURVE VALUES]
#    Again both ARVA and fixed WD decumulation.
# =============================================================================

cat("\n--- Graph 2: Ladder length sweep ---\n")

ladder_seq <- 5:15                                   # [ASSUMPTION: RANGE]
wd_curves_2 <- c(0.04, 0.06, 0.08)                    # [ASSUMPTION: CURVE VALUES]

g2_curves <- list()
for (wdv in wd_curves_2) {
  res_arva  <- data.frame(y = numeric(0), ruin_prob = numeric(0))
  res_fixed <- data.frame(y = numeric(0), ruin_prob = numeric(0))

  for (LL in ladder_seq) {
    bc <- get_bond_cost(pot, wdv, LL, convexity_buffer)
    if (!bc$feasible) next
    equity0 <- pot - bc$total_cost

    term_arva  <- grow_and_decumulate(equity0, LL, wdv, "ARVA",     inflation_rate,
                                      start_age, max_age, horizon, equity_monthly_returns, pot)
    term_fixed <- grow_and_decumulate(equity0, LL, wdv, "fixed_wd", inflation_rate,
                                      start_age, max_age, horizon, equity_monthly_returns, pot)

    bq_tgt <- bequeathment_target(BASE_BEQUEATH_PCT)
    res_arva  <- rbind(res_arva,  data.frame(y = LL, ruin_prob = mean(term_arva  < bq_tgt)))
    res_fixed <- rbind(res_fixed, data.frame(y = LL, ruin_prob = mean(term_fixed < bq_tgt)))
  }

  g2_curves[[sprintf("wd %.0f%% - ARVA", wdv * 100)]]     <- res_arva
  g2_curves[[sprintf("wd %.0f%% - Fixed WD", wdv * 100)]] <- res_fixed
  cat(sprintf("  wd = %.0f%% done\n", wdv * 100))
}

plot_ruin_sensitivity(
  g2_curves,
  xlab = "Probability of Ruin (%)",
  ylab = "Ladder Length (years)",
  main = "Sensitivity: Ladder Length vs. Probability of Ruin",
  filename = "02_ladder_length.png"
)

# =============================================================================
# 7. GRAPH 3 - BOND PORTFOLIO COST (in general, NOT Redington-derived)
#    Instead of letting Redington size the bond ladder, the bond cost is set
#    directly (R0 up to R9.5m in R100k / 1%-of-pot steps) and equity gets the
#    remainder - exactly your "1m so equity has 9m, then 1.1m and 8.9m..."
#    example.  [ASSUMPTION: RANGE]
#    Curves: withdrawal rate in {4%, 6%, 8%}, ladder length held at
#    BASE_LADDER_YEARS (just sets how long the equity slug grows before
#    decumulation starts - the bond ladder itself is still ignored).
#    Decumulation: ARVA only.  [ASSUMPTION: DECUMULATION]
# =============================================================================

cat("\n--- Graph 3: Bond portfolio cost sweep ---\n")

bond_cost_seq <- seq(0, 0.95 * pot, by = 0.01 * pot)   # [ASSUMPTION: RANGE]
wd_curves_3   <- c(0.04, 0.06, 0.08)                   # [ASSUMPTION: CURVE VALUES]

g3_curves <- list()
for (wdv in wd_curves_3) {
  res <- data.frame(y = numeric(0), ruin_prob = numeric(0))

  for (bcost in bond_cost_seq) {
    equity0 <- pot - bcost
    term <- grow_and_decumulate(equity0, BASE_LADDER_YEARS, wdv, "ARVA", inflation_rate,
                                start_age, max_age, horizon, equity_monthly_returns, pot)
    bq_tgt <- bequeathment_target(BASE_BEQUEATH_PCT)
    res <- rbind(res, data.frame(y = bcost / 1e6, ruin_prob = mean(term < bq_tgt)))
  }

  g3_curves[[sprintf("wd %.0f%%", wdv * 100)]] <- res
  cat(sprintf("  wd = %.0f%% done\n", wdv * 100))
}

plot_ruin_sensitivity(
  g3_curves,
  xlab = "Probability of Ruin (%)",
  ylab = "Bond Portfolio Cost (R millions)",
  main = sprintf("Sensitivity: Bond Portfolio Cost vs. Probability of Ruin\n(Pot = R%s, Ladder = %d yrs, ARVA decumulation)",
                 format(pot, big.mark = ","), BASE_LADDER_YEARS),
  filename = "03_bond_cost.png"
)

# =============================================================================
# 8. GRAPH 4 - DECISION MATRIX USED vs. NOT USED
#    Uses the REAL run_dynamic_ladder_simulation() (full ladder mechanics,
#    coupons, cash buffer, extend/defend, ARVA) via a small toggle wrapper
#    that forces "Wait & Hold" every year when the decision matrix is off -
#    i.e. the ladder is bought once at t = 0 and left alone until it matures
#    or hits the cap, at which point it's liquidated into equity exactly as
#    before. x = P(Ruin), y = withdrawal rate 2%-10% by 1%, ladder length
#    held at BASE_LADDER_YEARS.   [ASSUMPTION: RANGE / BASE CASE]
# =============================================================================

# ---- 8a. Toggle wrapper: identical to run_dynamic_ladder_simulation(), with
# one addition - use_decision_matrix. When FALSE, the annual check always
# resolves to "Wait & Hold" (no extend, no defend, no rebalance), which is
# exactly "set the ladder at t = 0 and don't touch it".
run_dynamic_ladder_simulation_toggle <- function(
    pot, wd, ladder_length, max_ladder_years, extend_by,
    bonds_fixed, valuation_date, convexity_buffer,
    equity_monthly_returns, horizon,
    cash_annual_rate, defend_cut, inflation_rate,
    start_age, max_age,
    res_redington_initial, C0_initial, bond_cf_initial,
    use_decision_matrix = TRUE
) {

  num_sims <- ncol(equity_monthly_returns)
  n_bonds  <- nrow(res_redington_initial$bond_metrics)
  bond_codes <- res_redington_initial$bond_metrics$bond_code
  cash_monthly_rate <- (1 + cash_annual_rate)^(1/12) - 1

  phase                <- rep("ladder", num_sims)
  ladder_years_current <- rep(ladder_length, num_sims)
  current_income       <- rep(wd * pot, num_sims)
  infeasible_flag       <- rep(FALSE, num_sims)
  bond_sale_amount      <- rep(NA_real_, num_sims)
  duration_matched_flag <- rep(TRUE, num_sims)
  ladder_end_month      <- rep(NA_integer_, num_sims)
  arva_year_index       <- rep(0L, num_sims)

  bond_holdings <- matrix(0, nrow = num_sims, ncol = n_bonds,
                          dimnames = list(NULL, bond_codes))
  bond_holdings[, names(res_redington_initial$bond_units)] <-
    matrix(res_redington_initial$bond_units, nrow = num_sims, ncol = n_bonds, byrow = TRUE)

  cash_account <- rep(C0_initial, num_sims)
  EPort        <- rep((pot - res_redington_initial$total_cost - C0_initial), num_sims)

  deposits_by_month <- matrix(0, nrow = horizon, ncol = num_sims)
  init_deposits <- bond_cf_initial$amount
  init_months   <- pmin(pmax(bond_cf_initial$month_index, 1), horizon)
  for (k in seq_along(init_deposits)) {
    deposits_by_month[init_months[k], ] <- deposits_by_month[init_months[k], ] + init_deposits[k]
  }

  EPort_history           <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_history            <- matrix(0, nrow = horizon + 1, ncol = num_sims)
  Cash_deposit_history    <- matrix(0, nrow = horizon,     ncol = num_sims)
  Cash_withdrawal_history <- matrix(0, nrow = horizon,     ncol = num_sims)
  ARVA_withdrawal_history <- matrix(NA_real_, nrow = 30, ncol = num_sims)

  EPort_history[1, ] <- EPort
  Cash_history[1, ]  <- cash_account

  current_monthly_withdrawal <- current_income / 12

  for (m in 1:horizon) {

    is_anniversary <- (m > 12) && ((m - 1) %% 12 == 0)
    ladder_idx <- which(phase == "ladder")

    bond_metrics_now <- NULL
    if (is_anniversary) {
      valuation_date_now <- valuation_date + months(m - 1)
      bond_metrics_now <- calculate_bond_metrics(bonds_fixed, valuation_date_now)
    }

    if (is_anniversary && length(ladder_idx) > 0) {
      for (s in ladder_idx) {

        bond_value_now <- mark_to_model_bond_value(bond_holdings[s, ], valuation_date_now, bond_metrics_now)
        remaining_years <- ladder_years_current[s] - (m - 1) / 12

        if (remaining_years <= 1e-9) {
          EPort[s] <- EPort[s] + bond_value_now
          bond_sale_amount[s] <- bond_value_now
          bond_holdings[s, ] <- 0
          phase[s] <- "arva"
          ladder_end_month[s] <- m - 1
          next
        }

        if (infeasible_flag[s]) next

        age_now <- start_age + (m - 1) / 12
        remaining_lifetime_years <- max_age - age_now
        fr <- compute_funded_ratio(
          equity_value             = EPort[s],
          cash_value                = cash_account[s],
          bond_value                = bond_value_now,
          expected_annual_expense   = current_income[s],
          horizon_years             = remaining_lifetime_years
        )
        trailing_ret <- prod(equity_monthly_returns[(m - 12):(m - 1), s]) - 1

        # ---- THE TOGGLE ----
        dec <- if (use_decision_matrix) {
          decision_matrix(
            funded_ratio    = fr$funded_ratio,
            equity_return   = trailing_ret,
            inflation_rate  = inflation_rate,
            current_income  = current_income[s],
            defend_cut      = defend_cut
          )
        } else {
          list(action = "Wait & Hold", extend_ladder = FALSE, withdrawal = current_income[s])
        }
        # ---------------------

        if (dec$action %in% c("Wait & Hold", "Repair")) next

        new_ladder_years <- ladder_years_current[s]
        if (dec$extend_ladder) {
          new_ladder_years <- min(ladder_years_current[s] + extend_by, max_ladder_years)
        }
        new_remaining_years <- new_ladder_years - (m - 1) / 12

        rb <- rebalance_ladder(
          remaining_years      = new_remaining_years,
          annual_withdrawal    = dec$withdrawal,
          valuation_date_now   = valuation_date_now,
          current_bond_value   = bond_value_now,
          current_cash_value   = cash_account[s],
          convexity_buffer     = convexity_buffer,
          cash_annual_rate     = cash_annual_rate,
          bond_metrics_precomputed = bond_metrics_now
        )

        if (!rb$feasible) {
          infeasible_flag[s] <- TRUE
          next
        }

        duration_matched_flag[s] <- rb$duration_matched
        EPort[s] <- EPort[s] - rb$harvest_needed
        cash_account[s] <- rb$new_C0
        bond_holdings[s, ] <- 0
        bond_holdings[s, names(rb$bond_units)] <- rb$bond_units

        deposits_by_month[m:horizon, s] <- 0
        new_deposits <- rb$bond_cf$amount
        new_months   <- pmin(pmax(rb$bond_cf$month_index + (m - 1), m), horizon)
        for (k in seq_along(new_deposits)) {
          deposits_by_month[new_months[k], s] <- deposits_by_month[new_months[k], s] + new_deposits[k]
        }

        ladder_years_current[s] <- new_ladder_years
        current_income[s] <- dec$withdrawal
        current_monthly_withdrawal[s] <- dec$withdrawal / 12
      }
    }

    for (s in 1:num_sims) {
      if (phase[s] == "ladder") {
        cash_account[s] <- cash_account[s] + deposits_by_month[m, s]
        cash_account[s] <- cash_account[s] - current_monthly_withdrawal[s]
        Cash_deposit_history[m, s]    <- deposits_by_month[m, s]
        Cash_withdrawal_history[m, s] <- current_monthly_withdrawal[s]
      } else {
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

    EPort <- EPort * equity_monthly_returns[m, ]
    EPort_history[m + 1, ] <- EPort
  }

  list(
    EPort_history      = EPort_history,
    ladder_end_month   = ladder_end_month,
    ladder_years_final = ladder_years_current,
    infeasible_flag    = infeasible_flag,
    EPort              = EPort
  )
}

# ---- 8b. Sweep ----
cat("\n--- Graph 4: Decision matrix on vs. off sweep (full dynamic simulation - slower) ---\n")

wd_seq_dm <- seq(0.02, 0.10, by = 0.01)   # [ASSUMPTION: RANGE - coarser, full sim is expensive]

g4_curves <- list(
  "Decision Matrix ON"  = data.frame(y = numeric(0), ruin_prob = numeric(0)),
  "Decision Matrix OFF" = data.frame(y = numeric(0), ruin_prob = numeric(0))
)

for (wdv in wd_seq_dm) {

  res_redington <- tryCatch(
    optimize_redington_immunization(
      total_pot_value = pot, withdrawal_rate_pct = wdv * 100,
      ladder_years = BASE_LADDER_YEARS, convexity_buffer = convexity_buffer,
      on_infeasible = "flag"
    ),
    error = function(e) list(feasible = FALSE)
  )
  if (!isTRUE(res_redington$feasible) || res_redington$total_cost > pot) {
    cat(sprintf("  wd = %.0f%%: infeasible bond ladder, skipped\n", wdv * 100))
    next
  }

  bond_cf <- bond_cashflow_schedule(res_redington$portfolio_allocation, bonds_fixed, valuation_date)
  cash_buffer <- size_ladder_cash_buffer(
    bond_cf = bond_cf, ladder_years = BASE_LADDER_YEARS,
    annual_withdrawal = wdv * pot, cash_annual_rate = cash_annual_rate
  )
  C0 <- cash_buffer$C0
  if (res_redington$total_cost + C0 > pot) {
    cat(sprintf("  wd = %.0f%%: infeasible (ladder + cash buffer > pot), skipped\n", wdv * 100))
    next
  }

  bq_tgt <- bequeathment_target(BASE_BEQUEATH_PCT)

  for (toggle in c(TRUE, FALSE)) {
    sim_out <- run_dynamic_ladder_simulation_toggle(
      pot = pot, wd = wdv, ladder_length = BASE_LADDER_YEARS,
      max_ladder_years = max_ladder_years, extend_by = extend_by,
      bonds_fixed = bonds_fixed, valuation_date = valuation_date,
      convexity_buffer = convexity_buffer,
      equity_monthly_returns = equity_monthly_returns, horizon = horizon,
      cash_annual_rate = cash_annual_rate, defend_cut = defend_cut,
      inflation_rate = inflation_rate, start_age = start_age, max_age = max_age,
      res_redington_initial = res_redington, C0_initial = C0, bond_cf_initial = bond_cf,
      use_decision_matrix = toggle
    )
    ruin_prob <- mean(sim_out$EPort < bq_tgt)
    curve_name <- if (toggle) "Decision Matrix ON" else "Decision Matrix OFF"
    g4_curves[[curve_name]] <- rbind(g4_curves[[curve_name]],
                                     data.frame(y = wdv * 100, ruin_prob = ruin_prob))
  }
  cat(sprintf("  wd = %.0f%% done\n", wdv * 100))
}

plot_ruin_sensitivity(
  g4_curves,
  xlab = "Probability of Ruin (%)",
  ylab = "Withdrawal Rate (%)",
  main = sprintf("Sensitivity: Decision Matrix Used vs. Not Used\n(Ladder length = %d yrs)", BASE_LADDER_YEARS),
  filename = "04_decision_matrix.png"
)

# =============================================================================
# 9. GRAPH 5 - BEQUEATHMENT TARGET
#    Since bequeathment_pct only changes the RUIN THRESHOLD (not the
#    simulation itself), the terminal-value vector is computed ONCE per wd
#    curve and re-used across the whole bequeathment sweep - much cheaper
#    than re-simulating.
#    Curves: withdrawal rate in {4%, 6%, 8%}, ladder length held at
#    BASE_LADDER_YEARS, ARVA decumulation only.  [ASSUMPTION: DECUMULATION]
# =============================================================================

cat("\n--- Graph 5: Bequeathment target sweep ---\n")

bequeath_seq <- seq(0, 0.30, by = 0.02)               # [ASSUMPTION: RANGE]
wd_curves_5  <- c(0.04, 0.06, 0.08)                   # [ASSUMPTION: CURVE VALUES]

g5_curves <- list()
for (wdv in wd_curves_5) {
  bc <- get_bond_cost(pot, wdv, BASE_LADDER_YEARS, convexity_buffer)
  if (!bc$feasible) { cat(sprintf("  wd = %.0f%%: infeasible, skipped\n", wdv * 100)); next }
  equity0 <- pot - bc$total_cost

  term <- grow_and_decumulate(equity0, BASE_LADDER_YEARS, wdv, "ARVA", inflation_rate,
                              start_age, max_age, horizon, equity_monthly_returns, pot)

  res <- data.frame(
    y = bequeath_seq * 100,
    ruin_prob = sapply(bequeath_seq, function(bq) mean(term < bequeathment_target(bq)))
  )
  g5_curves[[sprintf("wd %.0f%%", wdv * 100)]] <- res
  cat(sprintf("  wd = %.0f%% done\n", wdv * 100))
}

plot_ruin_sensitivity(
  g5_curves,
  xlab = "Probability of Ruin (%)",
  ylab = "Bequeathment Target (% of Starting Pot)",
  main = sprintf("Sensitivity: Bequeathment Target vs. Probability of Ruin\n(Ladder = %d yrs, ARVA decumulation)", BASE_LADDER_YEARS),
  filename = "05_bequeathment.png"
)

# =============================================================================
# TOTAL RUNTIME
# =============================================================================
script_end_time <- Sys.time()
cat("\n=======================================================\n")
cat(sprintf("Total script runtime: %s\n", format(script_end_time - script_start_time)))
cat("=======================================================\n")
