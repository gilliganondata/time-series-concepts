
set.seed(61705)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               scales,
               lubridate)



# Function to generate time-series with different characteristics
get_ts <- function(start = Sys.Date()-90, 
                   end = Sys.Date() - 1,
                   increment = "day",
                   seasonality = 7,
                   base = 10000, # base/starting number
                   trend = 0,  # -1 to 1; 1 is steepest positive trend
                   noise_level = 0.1,  # 0 = no noise; 1 = max noise
                   intervention_date = NA,
                   intervention_effect = NA # -1 to 1
){
  
  dates = seq.Date(start, end, increment)
  values = runif(length(dates),
                 base * (1 - noise_level),
                 base * (1 + noise_level)) |> 
    round()
  
  df <- data.frame(date = dates,
                   values = values)
}

test <- get_ts()