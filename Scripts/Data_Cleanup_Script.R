library(dplyr)
library(tidyr)

#splitting the life expectancy range column into two (a min and max) so the formatting is numeric instead of string 
SeparatedRangeData <- Life_Expectancy_Data_2010_2015 %>% 
  filter(complete.cases(.)) %>%    
  separate(`Life Expectancy Range`, into = c("Range_Min", "Range_Max"), sep = "-", convert = TRUE)

#averaging all of the data for each county so there is one row per county 
ExpectancyData_clean <- SeparatedRangeData %>%
  group_by(County) %>%
  summarise(
    Avg_Life_Expectancy    = mean(`Life Expectancy`,                na.rm = TRUE),
    Avg_Range_Min          = mean(Range_Min,                        na.rm = TRUE),
    Avg_Range_Max          = mean(Range_Max,                        na.rm = TRUE),
    Avg_Life_Expectancy_SE = mean(`Life Expectancy Standard Error`, na.rm = TRUE),
    Tract_Count            = n(),
    Valid_Entries          = sum(!is.na(`Life Expectancy`))
  ) %>%
  ungroup() 

#Here I am formatting the data so that the county and state are in separate columns and the state is the first column
ExpectancyData_clean <- ExpectancyData_clean %>% separate(County, into = c("County", "State"), sep = ",") 
ExpectancyData_clean <- ExpectancyData_clean %>%
  select(State, County, everything())

#saving this new datatset as a csv to the folder 
write.csv(ExpectancyData_clean, "ExpectancyData_clean.csv", row.names = FALSE)

#now creating a state-level data set that has life expectancy data by state by merging and averaging all the county columns
StateData_clean <- ExpectancyData_clean %>%
  group_by(State) %>%
  summarise(
    Avg_Life_Expectancy    = mean(Avg_Life_Expectancy,    na.rm = TRUE),
    Avg_Range_Min          = mean(Avg_Range_Min,          na.rm = TRUE),
    Avg_Range_Max          = mean(Avg_Range_Max,          na.rm = TRUE),
    Avg_Life_Expectancy_SE = mean(Avg_Life_Expectancy_SE, na.rm = TRUE),
    County_Count           = n()
  ) %>%
  ungroup()

View(StateData_clean)
write.csv(StateData_clean, "LifeExpectancyStateData_clean.csv", row.names = FALSE)

#cleaning up Parkinsons Data
Parkinsons_mortality_rates <- Parkinsons_mortality_rates %>%
  select(-URL)

#averages all the years together so these data points in the table now have average deaths and death rate per state for the years 2014-2023
Parkinsons_mortality_rates_clean <- Parkinsons_mortality_rates %>%
  group_by(Location) %>%
  summarise(
    Avg_Death_Rate = mean(`Death Rate`, na.rm = TRUE),
    Avg_Deaths     = mean(Deaths,       na.rm = TRUE)
  ) %>%
  ungroup()
Parkinsons_mortality_rates_clean <- Parkinsons_mortality_rates_clean[-9, ]

View(Parkinsons_mortality_rates_clean)
write.csv(Parkinsons_mortality_rates_clean, "Clean Datasets/Parkinsons_mortality_rates_clean.csv", row.names = FALSE)

Farm_Data <- Farm_Data %>%
  arrange(State) %>% select(-X, -X.1, -X.2, -X.3, -X.4)
write.csv(Farm_Data, "Clean Datasets/Farm_Data_2024.csv", row.names = FALSE)

Parkinson_Data <- Parkinson_Data[-9, ]
View(Parkinson_Data_1)

# creating a state wide pesticide dataset