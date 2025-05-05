# Load helpers
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
source("estimate_meals_mc.R")

# Constants and experiment variables
ACTUAL_TOTAL_MEALS <- 18.15
NUM_ITERATIONS <- 10000
set.seed(20250121)

# Run experiment
experiment_data <- estimate_meals(NUM_ITERATIONS)

analysis_data <- data.frame(Iteration_Num = experiment_data$Iteration_Num,
                            Cum_Meals_Total = cumsum(experiment_data$Meals_Total),
                            Mean_Meals_Total = cumsum(experiment_data$Meals_Total) / experiment_data$Iteration_Num,
                            Iteration_Bottleneck = ifelse(experiment_data$Meals_Holofoil >= experiment_data$Meals_Standard, 
                                                          "Holofoil",
                                                          "Standard"))

head(experiment_data)
head(analysis_data)

summary(experiment_data)

# Plots
# Distributions of total meals
ggplot(experiment_data, aes(x=Meals_Total)) + 
  geom_histogram(color="darkblue", fill="lightblue", binwidth=1) + 
  scale_x_continuous(breaks = pretty_breaks(max(experiment_data$Meals_Total) / 5)) +
  scale_y_continuous(breaks = pretty_breaks(10)) +
  labs(title = "Distribution of Total Meals Needed for All 15 Cards",
       x = "Number of Meals",
       y = "Count")

# Distribution of Meals Needed to Complete Standard vs Holofoil Sets
meal_types_df <- experiment_data %>% 
  select(Meals_Holofoil, Meals_Standard) %>%
  pivot_longer(cols = everything(),
               names_to = "Card_Type",
               values_to = "Count")

ggplot(meal_types_df, aes(x = Card_Type, y = Count, color = Count, fill = Card_Type)) + 
  geom_boxplot(outlier.color = "red", outlier.shape = 8) + 
  labs(title = "Meals Distribution Needed by Card Type",
       x = "Card Type",
       y = "Number of Meals")

# Iterations vs estimated mean # of meals
ggplot(analysis_data, aes(x = Iteration_Num, y = Mean_Meals_Total)) +
  geom_line(color = "darkblue") +
  geom_hline(yintercept = ACTUAL_TOTAL_MEALS, linetype = "dashed", color = "red") +
  ylim(c(min(analysis_data$Mean_Meals_Total), max(analysis_data$Mean_Meals_Total))) +
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(title = "Approximation of Total Meals Needed for All 15 Cards",
       x = "Number of Iterations",
       y = "Estimated Mean Number of Meals")

# Bottlenecks
bottlenecks_freqs <- as.data.frame(table(unlist(analysis_data$Iteration_Bottleneck)))
colnames(bottlenecks_freqs) <- c("Card_Type", "Count")
bottlenecks_freqs$Percent <- round(bottlenecks_freqs$Count / sum(bottlenecks_freqs$Count) * 100, 1)
bottlenecks_freqs$Label <- paste0(bottlenecks_freqs$Percent, "%")

ggplot(bottlenecks_freqs, aes(x = "", y = Count, fill = Card_Type)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5),
            color = "black", size = 4) +
  guides(fill = guide_legend(title = "Card Type")) + 
  scale_fill_brewer(palette = "Blues") +
  theme_void() + 
  labs(title = "Percentage of Bottleneck Types")