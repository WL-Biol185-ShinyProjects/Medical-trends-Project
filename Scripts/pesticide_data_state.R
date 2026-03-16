library(dplyr)

# Load the data
pesticides_2015 <- read.csv("pesticides_by_country.csv", stringsAsFactors = FALSE)

result <- read.csv("pesticides_by_county.csv") |>
  filter(year == 2015) |>
  group_by(state_name) |>
  summarise(
    LOW_ESTIMATE  = sum(LOW_ESTIMATE,  na.rm = TRUE),
    HIGH_ESTIMATE = sum(HIGH_ESTIMATE, na.rm = TRUE)
  )

write.csv(pesticides_by_county, "pesticides_by_county.csv", row.names = FALSE)