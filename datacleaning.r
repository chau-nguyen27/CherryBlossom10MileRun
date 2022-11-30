library('tidyverse')
library('tidyr')

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
    # setting age as numeric
    mutate(Age = as.numeric(Age))
  
  # splitting time and pace into HMS values
  df <- df %>% 
    separate(Time, into = c("T_Hour", "T_Min", "T_Sec"), remove = FALSE) %>% 
    separate(Pace, into = c("P_Min", "P_Sec")) %>% 
    # we are observing only 10M length races, so we can split the year and take that out
    separate(Race, into = c("Year", "Race_Length")) 
  
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
             "PiD.TiD")]
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

## Merging weather data (?)

## Identify same runners across races (might need to use "elite runners" records)

