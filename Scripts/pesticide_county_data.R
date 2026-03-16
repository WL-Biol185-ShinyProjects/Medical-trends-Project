library(dplyr)

# Load the data
pesticides_2015 <- read.csv("Pesticide_Data_2015.csv", stringsAsFactors = FALSE)
dictionary      <- read.csv("dictionary.csv",           stringsAsFactors = FALSE)

# Rename key columns
pesticides_2015 <- rename(pesticides_2015, compound = COMPOUND, state_code = STATE_CODE, county_code = COUNTY_CODE)
dictionary      <- rename(dictionary, state_code = STATE_CODE, county_code = COUNTY_CODE, county_name = COUNTY, state_name = STATE)

# Merge county and state names into pesticide data
pesticides_2015 <- merge(pesticides_2015, dictionary, by = c("state_code", "county_code"), all.x = TRUE)

# Filter to specific compounds
compounds_to_keep <- c("2,4-D", "Glyphosate", "Paraquat", "Malathion", "Chlorpyrifos")
pesticides_2015_filtered <- subset(pesticides_2015, compound %in% compounds_to_keep)

# Check the result
# head(pesticides_2014_filtered)
# head(pesticides_2015_filtered)

write.csv(pesticides_2015_filtered, "pesticides_2015_filtered.csv", row.names = FALSE)


# Add a year column to each so you can tell them apart
pesticides_2015_filtered$year <- 2015

# Condense pesticide data by county
pesticides_by_county <- pesticides_2015_filtered %>%
  group_by(state_code, state_name, county_code, county_name, compound) %>%
  summarise(
    LOW_ESTIMATE  = sum(LOW_ESTIMATE,  na.rm = TRUE),
    HIGH_ESTIMATE = sum(HIGH_ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  )

# add column with average of low and high estimates

average_estimate <- pesticides_by_county %>%
  group_by(state_name, state_code, county_code, county_name, compound) |>
  summarise(
    LOW_ESTIMATE  = sum(LOW_ESTIMATE,  na.rm = TRUE),
    HIGH_ESTIMATE = sum(HIGH_ESTIMATE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    AVG_ESTIMATE = (LOW_ESTIMATE + HIGH_ESTIMATE) / 2
  )

# Save the result
write.csv(average_estimate, "county_pesticides_data.csv", row.names = FALSE)
