###############################################################
# Retirement Decision Matrix
#
# Inputs:
#   funded_ratio    = Assets / PV of ALL future expected retirement
#                      expenses (now through max_age) - not just the cost
#                      of fully funding the current bond ladder
#   equity_return   = Previous 12-month equity return (decimal)
#   inflation_rate  = Expected inflation (decimal). Used ONLY to classify the
#                      state; it no longer escalates income - see below.
#   current_income  = Current annual withdrawal, a T0-REAL level
#   defend_cut      = Spending cut in Defend state (default = 5%)
#
# Outputs:
#   A list containing:
#     - decision state
#     - withdrawal for next year
#     - whether to extend ladder
#     - whether to harvest equities
#     - whether inflation increase is applied
#
# INCOME IS REAL, AND ONLY DEFEND MOVES IT
#
# `current_income` is a T0-REAL annual level and the engine indexes the actual
# monthly payment by realized CPI since t0 (see run_dynamic_ladder_simulation()).
# Purchasing power is therefore already preserved without this function doing
# anything, so Extend & Harvest returns the income UNCHANGED: it extends the
# ladder and harvests equity to pay for it, and that is all.
#
# Extend & Harvest used to return current_income * (1 + inflation_rate). That
# cancelled against an `income_reset_month` in the engine which re-based the CPI
# indexation on every reset - so the raise was inflation compensation, not a real
# raise, but ONLY when a reset fired every 12 months AND realized inflation was
# exactly inflation_rate. Off either condition the real income drifted with no
# decision having been taken (at 10% realized inflation it eroded 24% over seven
# years; a single Extend after three Hold years cut it 14% outright). The reset
# is gone and so is the raise; the two only ever made sense together.
###############################################################

decision_matrix <- function(funded_ratio,
                            equity_return,
                            inflation_rate,
                            current_income,
                            defend_cut = 0.05) {

  #-----------------------------------------
  # Determine portfolio state
  #-----------------------------------------

  surplus <- funded_ratio >= 1
  market_positive <- equity_return > 0

  #=========================================
  # 1. Extend & Harvest
  #=========================================

  if (surplus & market_positive) {

    return(list(
      action = "Extend & Harvest",

      funded_ratio = funded_ratio,
      equity_return = equity_return,

      extend_ladder = TRUE,
      harvest_equity = TRUE,

      # Income is untouched: it is a REAL level and the engine indexes it to
      # realized CPI from t0, so no raise is needed to preserve it.
      inflation_raise = FALSE,
      spending_cut = FALSE,

      withdrawal = current_income,

      notes = "Harvest equity gains and purchase the next rung of the bond ladder. Real income unchanged."
    ))
  }

  #=========================================
  # 2. Wait & Hold
  #=========================================

  if (surplus & !market_positive) {

    return(list(
      action = "Wait & Hold",

      funded_ratio = funded_ratio,
      equity_return = equity_return,

      extend_ladder = FALSE,
      harvest_equity = FALSE,

      inflation_raise = FALSE,
      spending_cut = FALSE,

      withdrawal = current_income,

      notes = "Do not extend ladder. Use existing bond ladder and avoid selling equities."
    ))
  }

  #=========================================
  # 3. Repair
  #=========================================

  if (!surplus & market_positive) {

    return(list(
      action = "Repair",

      funded_ratio = funded_ratio,
      equity_return = equity_return,

      extend_ladder = FALSE,
      harvest_equity = FALSE,

      inflation_raise = FALSE,
      spending_cut = FALSE,

      withdrawal = current_income,

      notes = "Allow equity growth to restore funded status before extending ladder."
    ))
  }

  #=========================================
  # 4. Defend
  #=========================================

  new_income <- current_income * (1 - defend_cut)

  return(list(
    action = "Defend",

    funded_ratio = funded_ratio,
    equity_return = equity_return,

    extend_ladder = FALSE,
    harvest_equity = FALSE,

    inflation_raise = FALSE,
    spending_cut = TRUE,

    defend_cut = defend_cut,

    withdrawal = new_income,

    notes = "Suspend ladder extension and temporarily reduce discretionary spending."
  ))
}
