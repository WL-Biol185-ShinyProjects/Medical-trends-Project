library(dplyr)

# Load the data
pesticides_2014 <- read.csv("Pesticide_Data_2014.csv", stringsAsFactors = FALSE)
pesticides_2015 <- read.csv("Pesticide_Data_2015.csv", stringsAsFactors = FALSE)
dictionary      <- read.csv("dictionary.csv",           stringsAsFactors = FALSE)

# Rename key columns
pesticides_2014 <- rename(pesticides_2014, compound = COMPOUND, state_code = STATE_CODE, county_code = COUNTY_CODE)
pesticides_2015 <- rename(pesticides_2015, compound = COMPOUND, state_code = STATE_CODE, county_code = COUNTY_CODE)
dictionary      <- rename(dictionary, state_code = STATE_CODE, county_code = COUNTY_CODE, county_name = COUNTY, state_name = STATE)

# Merge county and state names into pesticide data
pesticides_2014 <- merge(pesticides_2014, dictionary, by = c("state_code", "county_code"), all.x = TRUE)
pesticides_2015 <- merge(pesticides_2015, dictionary, by = c("state_code", "county_code"), all.x = TRUE)

# Filter to specific compounds
compounds_to_keep <- c("2,4-D", "Glyphosate", "Paraquat", "Malathion", "Maneb", "Metolachlor", "Chlorpyrifos","Rotenone", "Diquat", "Diazinon", "Acephate", "Dimethoate", "Mancozeb")
pesticides_2014_filtered <- subset(pesticides_2014, compound %in% compounds_to_keep)
pesticides_2015_filtered <- subset(pesticides_2015, compound %in% compounds_to_keep)

# Check the result
# head(pesticides_2014_filtered)
# head(pesticides_2015_filtered)

write.csv(pesticides_2014_filtered, "pesticides_2014_filtered.csv", row.names = FALSE)
write.csv(pesticides_2015_filtered, "pesticides_2015_filtered.csv", row.names = FALSE)

