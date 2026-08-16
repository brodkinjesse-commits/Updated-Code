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
#Depends only on age (via curve_yield, not on portfolio value or simulation path),
#so it can be precomputed once per year and reused across every simulation.
#Uses the same curve_yield() term structure as the Redington bond ladder
#(currently a flat 6% placeholder - see Reddington.R) so both pieces move
#together once a real fitted term structure replaces it.
arva_annuity_factor <- function(age, max_age = 90) {
  t <- 0:(max_age - age)
  ages_to_check <- age:(max_age - 1)
  one_year_probs <- sapply(ages_to_check, survival_probability)
  cum_survival <- c(1, cumprod(one_year_probs))   # tPx for t = 0, 1, 2, ...
  y_vec <- curve_yield(t) / 100
  sum(cum_survival / (1 + y_vec)^t)
}

#Runs the ARVA strategy for a SINGLE portfolio path over its own sequence of annual returns.
#Kept here as a standalone/reference implementation (e.g. for a single deterministic path
#or for testing) - not used directly in the vectorized Monte Carlo loop in Thesis.R,
#where withdrawals are computed across all simulations at once instead.
run_arva_strategy <- function(starting_capital, start_age, annual_returns, max_age = 90) {

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

    af <- arva_annuity_factor(age, max_age)

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
