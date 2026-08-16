##########################################################
# 3-Month Moving Block Bootstrap
##########################################################

bootstrap_returns <- function(Historical_Returns,
                              years = 30,
                              block_length = 3){
  
  #######################################################
  # Number of months needed
  #######################################################
  
  months_needed <- years * 12
  
  #######################################################
  # Number of blocks required
  #######################################################
  
  blocks_needed <- ceiling(months_needed / block_length)
  
  #######################################################
  # Number of historical observations
  #######################################################
  
  n <- nrow(Historical_Returns)
  
  #######################################################
  # Possible block starting positions
  #######################################################
  
  possible_starts <- 1:(n - block_length + 1)
  
  #######################################################
  # Empty dataframe
  #######################################################
  
  Simulated_Returns <- data.frame()
  
  #######################################################
  # Sample 3-month blocks
  #######################################################
  
  for(i in 1:blocks_needed){
    
    start <- sample(possible_starts,1)
    
    block <-
      Historical_Returns[start:(start + block_length - 1), ]
    
    Simulated_Returns <-
      rbind(Simulated_Returns, block)
    
  }
  
  #######################################################
  # Trim to exactly 360 months
  #######################################################
  
  Simulated_Returns <-
    Simulated_Returns[1:months_needed, ]
  
  #######################################################
  # Add simulation month
  #######################################################
  
  Simulated_Returns$Simulation_Month <- 1:nrow(Simulated_Returns)
  
  #######################################################
  # Add simulation year
  #######################################################
  
  Simulated_Returns$Simulation_Year <-
    ceiling(Simulated_Returns$Simulation_Month / 12)
  
  rownames(Simulated_Returns) <- NULL
  
  return(Simulated_Returns)
  
}