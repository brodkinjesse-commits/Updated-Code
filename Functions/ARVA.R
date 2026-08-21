#Import the mortality data
#Note i had to make a slight alteration to excel data - re uploaded corrected version
library(readxl)
data <- read_excel("Data/SAIML98_SAIFL98_Mortality_Table.xlsx")
data <- data[4:nrow(data), c(1, 5)]
data <- as.data.frame(data)

#Survival probability function - looks up 1-year mortality rate qx for a given age
#and returns the 1-year survival probability px
survival_probability <- function(age_now) {
  idx <- match(age_now, data$`Age (x)`)
  return(1 - data$`Male qx`[idx])
}

#Calculate the ARVA annuity factor for a given age
#
#NOMINAL by design, not a stray dependency: ARVA withdraws EPort / af(age)
#once a year, then pays that amount flat (no further escalation) for the
#next 12 months (see run_dynamic_ladder_simulation()'s ARVA cash mechanics).
#Since EPort is a nominal rand figure, af must be built off a NOMINAL
#discount curve for EPort/af to mean anything coherent - the real yield
#curve (Functions/Real_Yield_Curve.R) is deliberately NOT used here; that
#one only feeds the ILB bond selector's own real-terms world. curve_yield()
#(the old flat 6% Reddington-era placeholder) no longer exists anywhere in
#the codebase - replaced with the nominal PCA curve (Functions/
#yield_curve_simulation.R, yc$get_curve()) via real_curve_yield() (Functions/
#ILB_Repricing.R - a generic curve interpolator despite the name; nothing
#about it is real-specific).
#
#nominal_curve_vec / nominal_tenors_months: one (month, sim)'s nominal curve,
#e.g. yc$get_curve(month = m, sim = s) and yc$tenors_months - path-dependent,
#so must be supplied by the caller per path, not cached once per month the
#way the old flat curve allowed.
arva_annuity_factor <- function(age, nominal_curve_vec, nominal_tenors_months, max_age = 90) {
  t <- 0:(max_age - age)
  ages_to_check <- age:(max_age - 1)
  one_year_probs <- sapply(ages_to_check, survival_probability)
  cum_survival <- c(1, cumprod(one_year_probs))   # tPx for t = 0, 1, 2, ...
  y_vec <- real_curve_yield(t, nominal_curve_vec, nominal_tenors_months) / 100
  sum(cum_survival / (1 + y_vec)^t)
}

#Runs the ARVA strategy for a SINGLE portfolio path over its own sequence of annual returns.
#Kept here as a standalone/reference implementation (e.g. for a single deterministic path
#or for testing) - not used directly in the vectorized Monte Carlo loop in Thesis.R,
#where withdrawals are computed across all simulations at once instead.
#nominal_curve_vec/nominal_tenors_months applied UNCHANGED every year, as a
#simplification for this standalone tool - the real loop uses that year's
#own (month, sim) curve instead (see run_dynamic_ladder_simulation()).
run_arva_strategy <- function(starting_capital, start_age, annual_returns,
                              nominal_curve_vec, nominal_tenors_months, max_age = 90) {

  horizon <- length(annual_returns)
  age <- start_age
  portfolio <- starting_capital

  results <- data.frame(
    year            = 1:horizon,
    age             = NA_integer_,
    annuity_factor  = NA_real_,
    withdrawal      = NA_real_,
    portfolio_value = NA_real_
  )

  for (t in 1:horizon) {

    af <- arva_annuity_factor(age, nominal_curve_vec, nominal_tenors_months, max_age)

    withdrawal <- if (af > 0) portfolio / af else portfolio
    withdrawal <- min(withdrawal, portfolio)     # can't withdraw more than you have

    remainder <- portfolio - withdrawal
    portfolio <- remainder * (1 + annual_returns[t])

    results$age[t]             <- age
    results$annuity_factor[t]  <- af
    results$withdrawal[t]      <- withdrawal
    results$portfolio_value[t] <- portfolio

    age <- age + 1
  }

  return(results)
}
