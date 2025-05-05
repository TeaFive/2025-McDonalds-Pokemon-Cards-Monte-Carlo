# Run {#} iterations of buying Happy Meals until all
# fifteen cards are collected.
#
# @return - list of total meals, holofoil meals, and standard 
#           meals for one full simulation
source("prepare_data.R")

estimate_meals <- function(iterations) 
{
  summary_data <- data.frame(Iteration_Num = numeric(iterations),
                             Meals_Holofoil = numeric(iterations),
                             Meals_Standard = numeric(iterations),
                             Meals_Total = numeric(iterations))
  
  for (i in 1:iterations) {
    # Enumerated holofoil (1-7) and standard sets (1-8) 
    holofoils_collected <- rep(FALSE, 7)
    standards_collected <- rep(FALSE, 8)
    
    # Meal counts
    exp_meals_holofoil <- 0
    exp_meals_standard <- 0
    
    # Collect holofoils
    while (!all(holofoils_collected)) {
      pulled_cards <- sample(x=(1:7), size=1)
      holofoils_collected[pulled_cards] <- TRUE
      exp_meals_holofoil <- exp_meals_holofoil + 1
    }
    
    # Collect standards
    while (!all(standards_collected)) {
      pulled_cards <- sample(x=(1:8), size=3, replace=FALSE)
      standards_collected[pulled_cards] <- TRUE
      exp_meals_standard <- exp_meals_standard + 1
    }
    
    # Add current iteration's statistics to data frame
    summary_data$Iteration_Num[i] = i
    summary_data$Meals_Holofoil[i] = exp_meals_holofoil
    summary_data$Meals_Standard[i] = exp_meals_standard
    summary_data$Meals_Total[i] = max(exp_meals_holofoil, exp_meals_standard)
  }
  
  # Result
  return(summary_data)
}
