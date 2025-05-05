# Given data from one Monte Carlo simulation of n iterations, 
# prepare it in a data frame to be merged with the main data frame
#
# @param  raw_data - list of experimental number of total meals
# @return simulation_data - a data frame to bind to the main data frame
prepare_data <- function(raw_data)
{
  # Constant
  actual_mean <- 18.15
  
  # Prep data
  meals_total_list <- as.numeric(sapply(raw_data, function(x) x$meals_total)) 
  meals_holofoil_list <- sapply(raw_data, function(x) x$meals_holofoil)
  meals_standard_list <- sapply(raw_data, function(x) x$meals_standard)
  
  # Get meal values
  meals_total <- mean(meals_total_list)
  meals_holofoil <- mean(meals_holofoil_list)
  meals_standard <- mean(meals_standard_list)
  
  # Compute error statistics comparing simulation vs analytical expected value
  abs_error <- abs(meals_total - ACTUAL_MEAN)
  std_error <- sd(meals_total_list) / sqrt(n)
  rel_error <- abs((meals_total - ACTUAL_MEAN) / ACTUAL_MEAN) * 100
  variance <- var(meals_total_list)
  
  # Add experiment for n iterations to experiment data
  simulation_data <- data.frame(Num_Iterations = n,
                                Meals_Holofoil = meals_holofoil,
                                Meals_Standard = meals_standard,
                                Meals_Total = meals_total,
                                Abs_Error = abs_error,
                                Std_Error = std_error,
                                Rel_Error = rel_error,
                                Variance = variance)
  
  return(simulation_data)
}