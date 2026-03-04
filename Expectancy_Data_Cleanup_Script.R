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
