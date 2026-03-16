library(dplyr)

# Load the data
pesticides_data <- read.csv("county_pesticides_data_clean.csv", stringsAsFactors = FALSE)

# aggregate data by state
pesticides_by_state <- pesticides_data %>%
  group_by(state_code, state_name, compound) %>%
  summarise(
    LOW_ESTIMATE  = sum(LOW_ESTIMATE,  na.rm = TRUE),
    HIGH_ESTIMATE = sum(HIGH_ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  ) 

average_estimate <- pesticides_by_state %>%
  group_by(state_name, state_code, compound) |>
  summarise(
    LOW_ESTIMATE  = sum(LOW_ESTIMATE,  na.rm = TRUE),
    HIGH_ESTIMATE = sum(HIGH_ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    AVG_ESTIMATE = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
  )

write.csv(average_estimate, "state_pesticide_data_clean.csv", row.names = FALSE)