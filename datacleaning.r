library('tidyverse')
library('tidyr')
library('dplyr')

load("CBData_raw.RData")

# Check total number of observations
sum(sapply(CBData_raw, nrow))

## Reformatting variables
# Age, Time, Pace as numeric before combining data
to_numage <- function(df) {
  # first, remove rows with 'NR' in important columns
  df <- df %>% 
    filter(Age != 'NR') %>% 
    filter(Name != 'NR') %>% 
    filter(Time != 'NR') %>% 
    filter(Pace != 'NR') %>%
    # setting age as an integer
    mutate(Age = as.integer(Age))
  
  # splitting time and pace into HMS values
  df <- df %>% 
    separate(Time, into = c("T_Hour", "T_Min", "T_Sec"), remove = FALSE) %>% 
    separate(Pace, into = c("P_Min", "P_Sec")) %>% 
    # we are observing only 10M length races, so we can split the year and take that out
    separate(Race, into = c("Year", "Race_Length"))
  
  # now set those separated columns to integers
  df <- df %>% 
    mutate(T_Hour = as.integer(T_Hour)) %>%
    mutate(T_Min = as.integer(T_Min)) %>% 
    mutate(T_Sec = as.integer(T_Sec)) %>% 
    mutate(P_Min = as.integer(P_Min)) %>% 
    mutate(P_Sec = as.integer(P_Sec)) %>% 
    mutate(Year = as.integer(Year))
  
  # keeping most important columns to be used for analysis
  df <- df[c("Year",
             "Name",
             "Age",
             "T_Hour",
             "T_Min",
             "T_Sec",
             "P_Min",
             "P_Sec",
             "PiS.TiS",
             "PiD.TiD",
             "Hometown")]
}

# testing the 15th year of the race to see if it works
# test = to_numage(data.frame(CBData_raw[15]))

## Combining data
cleaned = data.frame()

# use the to_numage function on all years
# and combine them into one big dataframe
for (i in 1:47) {
  temp = to_numage(data.frame(CBData_raw[i]))
  cleaned = rbind(cleaned, temp)
}

# saving the cleaned data into a csv file
write.csv(cleaned, "cleaned_data_csv.csv", row.names = FALSE)

## Merging weather data (?)

## Identify same runners across races (might need to use "elite runners" records)
# matching racers by their name to get a list of potential repeating
# runners by using a combination of their hometown and the year-age
# Num_Entries is how many races they have been in
same_runners = cleaned %>% 
  group_by(Name) %>% 
  count(Name, Year-Age, Hometown, name = 'Num_Entries') %>% 
  arrange(desc(Num_Entries))

