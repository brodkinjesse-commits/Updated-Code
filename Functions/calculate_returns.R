library(dplyr)

##########################################################
# Calculate Monthly Returns
##########################################################

calculate_returns <- function(Bond_Equity_Data,
                              CPI_Data){
  
  #######################################################
  # Calculate Monthly Equity & Bond Returns
  #######################################################
  
  Asset_Returns <- Bond_Equity_Data %>%
    arrange(Date) %>%
    mutate(
      
      SA_Equity_Return =
        SA.Equity.ZAR / lag(SA.Equity.ZAR) - 1,
      
      SA_Bond_Return =
        SA.Bonds.ZAR / lag(SA.Bonds.ZAR) - 1
      
    )
  
  #######################################################
  # Calculate Monthly Inflation
  #######################################################
  
  Inflation_Data <- CPI_Data %>%
    arrange(Date) %>%
    mutate(
      
      Inflation =
        `CPI Index` / lag(`CPI Index`) - 1
      
    )
  
  #######################################################
  # Combine into one dataframe
  #######################################################
  
  Returns <- data.frame(
    
    Date = Asset_Returns$Date,
    
    SA_Equity_Return = Asset_Returns$SA_Equity_Return,
    
    SA_Bond_Return = Asset_Returns$SA_Bond_Return,
    
    Inflation = Inflation_Data$Inflation
    
  )
  
  #######################################################
  # Remove first row (contains NAs)
  #######################################################
  
  Returns <- na.omit(Returns)
  
  return(Returns)
  
}