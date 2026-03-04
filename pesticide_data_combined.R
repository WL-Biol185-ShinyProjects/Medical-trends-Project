library(dplyr)

# Load the data
pesticides_2014 <- read.csv("Pesticide_Data_2014.csv", stringsAsFactors = FALSE)
pesticides_2015 <- read.csv("Pesticide_Data_2015.csv", stringsAsFactors = FALSE)
dictionary       <- read.csv("dictionary.csv",       stringsAsFactors = FALSE)

pesticides_2014 <- pesticides_2014 %>%
  rename(compound = V1, state_code = V3, county_code = V4)

pesticides_2015 <- pesticides_2015 %>%
  rename(compound = V1, state_code = V3, county_code = V4)

# Rename relevant columns in dictionary
dictionary <- dictionary %>%
  rename(state_code = V1, county_code = V2, county_name = V3, state_name = V4)


pesticides_2014_named <- pesticides_2014 %>%
  left_join(dictionary, by = c("state_code", "county_code"))

pesticides_2015_named <- pesticides_2015 %>%
  left_join(dictionary, by = c("state_code", "county_code"))
