# Script for all Data analysis and statistical 
library(ggplot2)

Parkinson_Data <- read.csv("Clean Datasets/Parkinsons_mortality_rates_clean.csv", stringsAsFactors = FALSE)
Expectancy_Data <- read.csv("Clean Datasets/ExpectancyData_clean.csv", stringsAsFactors = FALSE)
Expectancy_State_Data <- read.csv("Clean Datasets/LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
Pesticide_County_Data  <- read.csv("Clean Datasets/pesticides_by_county.csv", stringsAsFactors = FALSE)
Farm_Data  <- read.csv("Clean Datasets/Farm_Data_2024.csv", stringsAsFactors = FALSE)

#Farm & Parkinsons Data Analysis (State)
FarmParkinsonData <- data.frame(State = Farm_Data$State, NumberOfFarms = Farm_Data$Number_Of_Farms, DeathRate = Parkinson_Data$Avg_Death_Rate)
regression <- lm(DeathRate ~ NumberOfFarms, data = FarmParkinsonData)
regression
summary(regression)

#Rsquared value
r_squared <- round(summary(regression)$r.squared, 3)

#correlation coefficient
cor_val <- cor(FarmParkinsonData$NumberOfFarms, FarmParkinsonData$DeathRate, use = "complete.obs")

#scatter plot 
scatter_plot <- ggplot(FarmParkinsonData, aes(x = NumberOfFarms, y = DeathRate, label = State)) +
  geom_point(color = "#2d5016", size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed", alpha = 0.2) +
  geom_text(vjust = -0.8, size = 2.8, color = "gray30") +
  labs(
    title = "Number of Farms vs. Parkinson's Avg Death Rate by State",
    x = "Number of Farms",
    y = "Avg Death Rate (Parkinson's)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(scatter_plot)