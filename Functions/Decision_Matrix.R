###############################################################
# Retirement Decision Matrix
#
# Inputs:
#   funded_ratio    = Assets / PV of ALL future expected retirement
#                      expenses (now through max_age) - not just the cost
#                      of fully funding the current bond ladder
#   equity_return   = Previous 12-month equity return (decimal)
#   inflation_rate  = Inflation for current year (decimal)
#   current_income  = Current annual withdrawal
#   defend_cut      = Spending cut in Defend state (default = 5%)
#
# Outputs:
#   A list containing:
#     - decision state
#     - withdrawal for next year
#     - whether to extend ladder
#     - whether to harvest equities
#     - whether inflation increase is applied
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

    new_income <- current_income * (1 + inflation_rate)

    return(list(
      action = "Extend & Harvest",

      funded_ratio = funded_ratio,
      equity_return = equity_return,

      extend_ladder = TRUE,
      harvest_equity = TRUE,

      inflation_raise = TRUE,
      spending_cut = FALSE,

      withdrawal = new_income,

      notes = "Harvest equity gains and purchase the next rung of the bond ladder."
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
