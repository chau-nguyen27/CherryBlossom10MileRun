load("CBData_raw.RData")

# Check total number of observations
sum(sapply(CBData_raw, nrow))

# Reformatting variables
## Age, Time, Pace as numeric before combining data
to_numage <- function(df) {
  df <- df %>% 
    mutate(Age = case_when(Age == 'NR' ~ '',
                           Age != "NR" ~ Age))
} 

## Combining data

## Create 'year' variable from `Race`

## Merging weather data (?)

## Identify same runners across races (might need to use "elite runners" records)

