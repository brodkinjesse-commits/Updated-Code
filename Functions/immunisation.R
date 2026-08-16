# ==============================================================================
# REDINGTON IMMUNIZATION MODEL (PV, DURATION, & CONVEXITY CONSTRAINTS)
# ==============================================================================
# Solves for an optimal bond portfolio meeting formal actuarial immunization:
# 1. PV(Assets) >= PV(Liabilities)
# 2. Macaulay Duration(Assets) == Macaulay Duration(Liabilities)
# 3. Convexity(Assets) >= Convexity(Liabilities) + Convexity Buffer
# ==============================================================================

if (!require("lpSolve")) install.packages("lpSolve")
if (!require("lubridate")) install.packages("lubridate")
library(lpSolve)
library(lubridate)

# ------------------------------------------------------------------------------
# 1. INPUT DATA: Fixed Income Bonds Dataset
# ------------------------------------------------------------------------------
bonds_fixed <- data.frame(
  bond_code = c("R188", "R2030", "R213", "R2033", "R2035", "R2038"),
  coupon_rate_nominal = c(10.50, 8.00, 7.00, 10.000, 8.875, 10.875), # % p.a.
  issue_date = as.Date(c("1998-05-22", "2013-10-04", "2010-05-28", "2024-09-13", "2015-07-17", "2024-09-13")),
  issue_price_pct = c(78.230, 95.261, 80.922, 104.700, 103.843, 104.710),
  interest_payment_date_1 = c("21/06", "28/02", "28/02", "31/03", "28/02", "31/03"),
  interest_payment_date_2 = c("21/12", "31/08", "31/08", "30/09", "31/08", "30/09"),
  redemption_date = as.Date(c("2027-12-21", "2030-01-31", "2031-02-28", "2033-03-31", "2035-02-28", "2038-03-31")),
  redemption_amount_pct = c(100, 100, 100, 100, 100, 100),
  years_to_maturity = c(1.38, 3.49, 4.57, 6.65, 8.57, 11.65),
  market_price = c(105.35684, 101.11087, 99.86363, 113.07027, 107.16969, 118.93882),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 2. ACTUARIAL METRICS CALCULATOR (PV, DURATION, CONVEXITY)
# ------------------------------------------------------------------------------
calculate_bond_metrics <- function(bonds_df, yield_pa) {
  y_semi <- yield_pa / 2 / 100 # Semi-annual yield to maturity
  n_bonds <- nrow(bonds_df)

  metrics <- data.frame(
    bond_code = bonds_df$bond_code,
    market_price = bonds_df$market_price,
    pv = numeric(n_bonds),
    mac_duration = numeric(n_bonds),
    convexity = numeric(n_bonds)
  )

  for (i in 1:n_bonds) {
    periods <- round(bonds_df$years_to_maturity[i] * 2)
    c_semi  <- bonds_df$coupon_rate_nominal[i] / 2
    par     <- bonds_df$redemption_amount_pct[i]

    t_vec <- 1:periods
    cf_vec <- rep(c_semi, periods)
    cf_vec[periods] <- cf_vec[periods] + par

    # Discount factors (semi-annual)
    df <- (1 + y_semi)^(-t_vec)

    # Present Value
    pv_bond <- sum(cf_vec * df)

    # Macaulay Duration (in Years) = (1 / 2) * [ sum(t * CF_t * df) / PV ]
    mac_dur_years <- 0.5 * (sum(t_vec * cf_vec * df) / pv_bond)

    # Annualized Convexity = [ sum(t * (t + 1) * CF_t * df) ] / [ 4 * PV * (1 + y_semi)^2 ]
    conv_annual <- sum(t_vec * (t_vec + 1) * cf_vec * df) / (4 * pv_bond * (1 + y_semi)^2)

    metrics$pv[i]           <- pv_bond
    metrics$mac_duration[i] <- mac_dur_years
    metrics$convexity[i]    <- conv_annual
  }

  return(metrics)
}

# ------------------------------------------------------------------------------
# 3. REDINGTON IMMUNIZATION OPTIMIZER
# ------------------------------------------------------------------------------
optimize_redington_immunization <- function(total_pot_value,
                                            withdrawal_rate_pct,
                                            ladder_years,
                                            yield_curve_pa = 9.0, # Assumed flat discount yield
                                            convexity_buffer = 0.5) { # Minimum C_A - C_L spread

  # A. Define Liability Cash Flows (Annual Retiree Withdrawals)
  annual_withdrawal <- total_pot_value * (withdrawal_rate_pct / 100)
  liability_years   <- 1:ladder_years
  liability_cfs     <- rep(annual_withdrawal, ladder_years)

  # B. Calculate Liability Present Value, Macaulay Duration, and Convexity
  y_annual <- yield_curve_pa / 100
  df_liab  <- (1 + y_annual)^(-liability_years)

  pv_liab  <- sum(liability_cfs * df_liab)

  # Liability Macaulay Duration = sum(t * PV_t) / PV_L
  mac_dur_liab <- sum(liability_years * liability_cfs * df_liab) / pv_liab

  # Liability Convexity = sum(t * (t + 1) * PV_t) / [ PV_L * (1 + y_annual)^2 ]
  conv_liab <- sum(liability_years * (liability_years + 1) * liability_cfs * df_liab) / (pv_liab * (1 + y_annual)^2)

  # C. Calculate Individual Bond Metrics
  bond_metrics <- calculate_bond_metrics(bonds_fixed, yield_curve_pa)
  n_bonds      <- nrow(bond_metrics)

  # D. Formulate Linear Program Constraints
  # Variable x_j = Units of Bond j to purchase

  # Objective: Minimize Total Upfront Purchase Cost sum(x_j * Market_Price_j)
  obj_costs <- bond_metrics$market_price

  # Constraint 1: PV(Assets) >= PV(Liabilities)
  # sum(x_j * PV_j) >= PV_L
  row_pv <- bond_metrics$pv

  # Constraint 2: Duration Match (D_A == D_L)
  # Linearized: sum(x_j * PV_j * (D_j - D_L)) == 0
  row_dur_eq <- bond_metrics$pv * (bond_metrics$mac_duration - mac_dur_liab)

  # Constraint 3: Convexity Spread (C_A >= C_L + Buffer)
  # Linearized: sum(x_j * PV_j * (C_j - (C_L + Buffer))) >= 0
  target_conv <- conv_liab + convexity_buffer
  row_conv    <- bond_metrics$pv * (bond_metrics$convexity - target_conv)

  # Combine constraints into matrix
  const_matrix <- rbind(
    PV_Match       = row_pv,
    Duration_Match = row_dur_eq,
    Convexity_Diff = row_conv
  )

  const_dir <- c(">=", "==", ">=")
  const_rhs <- c(pv_liab, 0, 0)

  # Solve LP
  lp_res <- lp(direction = "min",
               objective.in = obj_costs,
               const.mat = const_matrix,
               const.dir = const_dir,
               const.rhs = const_rhs)

  if (lp_res$status != 0) {
    stop("Linear Program failed to find a valid solution satisfying Redington conditions. Try relaxing the convexity buffer or broadening the bond selection.")
  }

  bond_units <- lp_res$solution
  names(bond_units) <- bond_metrics$bond_code

  # E. Calculate Portfolio Actuarial Summary
  total_cost    <- sum(bond_units * bond_metrics$market_price)
  total_pv_a    <- sum(bond_units * bond_metrics$pv)
  portfolio_dur <- sum(bond_units * bond_metrics$pv * bond_metrics$mac_duration) / total_pv_a
  portfolio_cnv <- sum(bond_units * bond_metrics$pv * bond_metrics$convexity) / total_pv_a

  # Construct Output Allocation Table
  allocation_table <- data.frame(
    Bond_Code = bond_metrics$bond_code,
    Price = bond_metrics$market_price,
    Units_Bought = round(bond_units, 4),
    Total_Cost = round(bond_units * bond_metrics$market_price, 2),
    Weight_Pct = round(((bond_units * bond_metrics$market_price) / total_cost) * 100, 2),
    Mac_Duration = round(bond_metrics$mac_duration, 2),
    Convexity = round(bond_metrics$convexity, 2)
  )

  actuarial_summary <- data.frame(
    Metric = c("Present Value (PV)", "Macaulay Duration (Years)", "Annualized Convexity"),
    Liabilities = round(c(pv_liab, mac_dur_liab, conv_liab), 4),
    Immunized_Portfolio = round(c(total_pv_a, portfolio_dur, portfolio_cnv), 4),
    Status = c(
      ifelse(total_pv_a >= pv_liab, "PASSED (PV_A >= PV_L)", "FAILED"),
      ifelse(abs(portfolio_dur - mac_dur_liab) < 0.001, "MATCHED (D_A == D_L)", "FAILED"),
      ifelse(portfolio_cnv > conv_liab, "PASSED (C_A > C_L)", "FAILED")
    )
  )

  return(list(
    total_cost = total_cost,
    actuarial_summary = actuarial_summary,
    portfolio_allocation = allocation_table[allocation_table$Units_Bought > 0, ]
  ))
}

# ------------------------------------------------------------------------------
# 4. EXECUTION
# ------------------------------------------------------------------------------
res_redington <- optimize_redington_immunization(
  total_pot_value = 10000000,
  withdrawal_rate_pct = 5.0,
  ladder_years = 7,
  yield_curve_pa = 9.0,      # 9.0% market discount rate
  convexity_buffer = 0.5      # C_A must exceed C_L by at least 0.5
)

cat("=======================================================\n")
cat("      REDINGTON IMMUNIZATION OPTIMIZATION RESULTS      \n")
cat("=======================================================\n\n")

cat("--- ACTUARIAL IMMUNIZATION VERIFICATION ---\n")
print(res_redington$actuarial_summary, row.names = FALSE)

cat("\n--- OPTIMAL BOND ALLOCATION ---\n")
print(res_redington$portfolio_allocation, row.names = FALSE)
cat(sprintf("\nTOTAL PORTFOLIO COST: R %.2f\n", res_redington$total_cost))

