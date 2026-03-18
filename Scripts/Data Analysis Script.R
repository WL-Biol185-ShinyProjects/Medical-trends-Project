# Script for all Data analysis and statistical 
library(ggplot2)
library(dplyr)

Parkinson_Data <- read.csv("Clean Datasets/Parkinsons_mortality_rates_clean.csv", stringsAsFactors = FALSE)
Expectancy_Data <- read.csv("Clean Datasets/ExpectancyData_clean.csv", stringsAsFactors = FALSE)
Expectancy_State_Data <- read.csv("Clean Datasets/LifeExpectancyStateData_clean.csv", stringsAsFactors = FALSE)
Pesticide_County_Data  <- read.csv("Clean Datasets/county_pesticides_data_clean.csv", stringsAsFactors = FALSE)
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




#//////////////////////// Pesticides vs Life Expectancy (County) ///////////////////////////

pesticides <- c("2,4-D", "Glyphosate", "Paraquat", "Chlorpyrifos")

# =============================================================================
# LOOP THROUGH EACH PESTICIDE
# =============================================================================

for (pesticide in pesticides) {
  
  cat("\n\n=====================================================\n")
  cat("  Pesticide:", pesticide, "\n")
  cat("=====================================================\n")
  
  # ---------------------------------------------------------------------------
  # FILTER + MERGE
  # ---------------------------------------------------------------------------
  
 # pest_subset <- county_pesticides_data_clean %>%
  #  filter(compound == pesticide) %>%         # update "compound" to your actual pesticide name column
   # select(county_name, AVG_ESTIMATE)
  
 # merged <- inner_join(pest_subset, Expectancy_Data, by = c("county_name" = "County")) %>%
  #  select(county_name, AVG_ESTIMATE, Avg_Life_Expectancy) %>%
   # filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Life_Expectancy))  # drop missing values
  
  # Average pesticide estimates per county first
  pest_subset <- county_pesticides_data_clean %>%
    filter(compound == pesticide) %>%
    select(county_name, AVG_ESTIMATE) %>%
    group_by(county_name) %>%
    summarise(AVG_ESTIMATE = mean(AVG_ESTIMATE, na.rm = TRUE))
  
  # Average life expectancy per county too
  expectancy_clean <- Expectancy_Data %>%
    group_by(County) %>%
    summarise(Avg_Life_Expectancy = mean(Avg_Life_Expectancy, na.rm = TRUE))
  
  # Now join clean data
  merged <- inner_join(pest_subset, expectancy_clean, by = c("county_name" = "County")) %>%
    filter(!is.na(AVG_ESTIMATE), !is.na(Avg_Life_Expectancy))
  
  if (nrow(merged) < 3) {
    cat("  Not enough data to analyze. Skipping.\n")
    next
  }
  
  # ---------------------------------------------------------------------------
  # SIDE BY SIDE TABLE
  # ---------------------------------------------------------------------------
  
  cat("\nSide-by-Side Table (first 10 rows):\n")
  print(head(merged, 10))
  
  # ---------------------------------------------------------------------------
  # CORRELATION
  # ---------------------------------------------------------------------------
  
  cor_result <- cor.test(merged$AVG_ESTIMATE, merged$Avg_Life_Expectancy)
  cor_val <- round(cor_result$estimate, 3)
  p_val <- round(cor_result$p.value, 4)
  
  cat("\nCorrelation Analysis:\n")
  print(cor_result)
  
  if (abs(cor_val) < 0.3) {
    cat("\nInterpretation: Weak correlation (r =", cor_val, ") — little to no linear relationship.\n")
  } else if (abs(cor_val) < 0.7) {
    cat("\nInterpretation: Moderate correlation (r =", cor_val, ") — some relationship exists.\n")
  } else {
    cat("\nInterpretation: Strong correlation (r =", cor_val, ") — substantial linear relationship detected.\n")
  }
  
  # ---------------------------------------------------------------------------
  # LINEAR REGRESSION
  # ---------------------------------------------------------------------------
  
  model <- lm(Avg_Life_Expectancy ~ AVG_ESTIMATE, data = merged)
  r_squared <- round(summary(model)$r.squared, 3)
  
  cat("\nLinear Regression Summary:\n")
  print(summary(model))
  cat("\nR² =", r_squared, "— AVG_ESTIMATE explains", r_squared * 100, 
      "% of variation in Avg Life Expectancy.\n")
  
  # ---------------------------------------------------------------------------
  # SCATTER PLOT
  # ---------------------------------------------------------------------------
  
  annotation_label <- paste0("r = ", cor_val, "\nR² = ", r_squared, "\np = ", p_val)
  
  p <- ggplot(merged, aes(x = AVG_ESTIMATE, y = Avg_Life_Expectancy)) +
    geom_point(color = "#2d5016", size = 2.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred", 
                linetype = "dashed", alpha = 0.2) +
    annotate("text",
             x = max(merged$AVG_ESTIMATE, na.rm = TRUE) * 0.75,
             y = min(merged$Avg_Life_Expectancy, na.rm = TRUE) * 1.02,
             label = annotation_label,
             size = 4, color = "darkred", fontface = "bold"
    ) +
    labs(
      title = paste0(pesticide, ": Pesticide Use vs. Avg Life Expectancy by County"),
      x = paste0(pesticide, " AVG_ESTIMATE"),
      y = "Avg Life Expectancy (years)"
    ) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold")) + scale_x_log10()
  
  print(p)
  
  # Save each plot as a separate PNG
  filename <- paste0(gsub(",| ", "_", pesticide), "_scatter.png")
  ggsave(filename, plot = p, width = 10, height = 7, dpi = 150)
  cat("\nPlot saved as:", filename, "\n")
}













