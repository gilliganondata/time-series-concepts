
set.seed(61705)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               tsibble,      # Use time-series tibbles
               gt,
               scales,
               lubridate,
               showtext,     # For using a custom font
               CausalImpact  # Bayesian structural time-series forecasting
)

# See https://r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
# The first argument can be changed to any Google font. The second argument
# should not be changed.
font_add_google("Raleway", family = "s_font")
showtext_auto()

# Bring in the xkcd font, Humor Sans (it needs to be installed on the system)
# The "how to actually do this" below is from:
# https://stackoverflow.com/questions/55933524/r-can-not-find-fonts-to-be-used-in-plotting
font_add("Humor Sans", "/Library/Fonts/Humor-Sans.ttf")
showtext_auto()

# Config settings for styling
# Palette from: https://www.learnui.design/tools/data-color-picker.html
s_bgrnd <- "transparent"
s_labels <- "gray90"
s_line_1 <- "#ffa600"
s_line_2 <- "#bc5090"
s_plot_title_size <- 24
s_axis_title_size <- 20
s_axis_text_size <- 18
      
# The main theme
theme_main <- theme_minimal() +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "s_font", face = "bold", size = s_plot_title_size, colour = s_labels),
        plot.background = element_rect(fill = s_bgrnd, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray40"),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "s_font", size = s_axis_text_size, colour = s_labels),
        axis.line.x = element_line(color = s_labels),
        axis.title.y = element_text(family = "s_font", size = s_axis_title_size, face = "bold", 
                                    colour = s_labels, margin = margin(0, 8, 0, 0, "pt")),
        axis.text.y = element_text(family = "s_font", size = s_axis_text_size, colour = s_labels),
        axis.line.y = element_blank(),
        # Go ahead and style the legend, even though it will be off most of the time
        legend.position = "none",
        legend.title = element_blank(),
        legend.text = element_text(family = "s_font", size = s_axis_title_size, 
                                   colour = s_labels, margin = margin(0, 10, 0, 0, "pt"))
  )

# Function to generate time-series with different characteristics and return a tsibble object.
# It could have just returned a data frame, but might as well get all tidy-proper
get_ts <- function(start = Sys.Date()-90, 
                   end = Sys.Date() - 1,
                   increment = "day",
                   week_season = FALSE,       # Only if increment is "day"; gives weekend dips
                   base = 10000,              # base/starting number
                   trend = 0,                 # The % change between the base and the final value
                   noise_level = 0.1,         # 0 = no noise; 1 = max noise
                   varying_variance = FALSE,  # How much the variance (noise level) varies over time
                                              # FALSE = constant variance; TRUE = variation in variance
                   intervention_date = NA,
                   intervention_effect = NA   # The % increase/decrease from the base expressed as a decimal
){
  
  # A bit of a hack, but a base of 0 is messy, so, if it's set to 0, make it close to 0
  if(base == 0) base <- 0.01
  
  # Set up the basic data frame
  dates = seq.Date(start, end, increment) 
  # Convert to the appropriate tsibble-friendly format
  if(increment == "day") {
    intervention_date <- as.Date(intervention_date)
  }
  if(increment == "week") {
    dates <- yearweek(dates)
    intervention_date <- yearweek(intervention_date)
  }
  if(increment == "month") {
    dates <-yearmonth(dates)
    intervention_date <- yearmonth(intervention_date)
  }
  if(increment == "quarter") {
    dates <-yearquarter(dates)
    intervention_date <- yearquarter(intervention_date)
  }
  if(increment == "year") {
    dates <-year(dates)
    intervention_date <- year(intervention_date)
  }
  
  # Set the base values. This is a "stationary with some noise" series of values
  # that get modified as needed based on the other arguments
  base_values = runif(length(dates),
                      base * (1 - noise_level),
                      base * (1 + noise_level)) |> 
    round()
  
  df <- tsibble(date = dates,
               value = base_values,
               index = date)
  
  # Add varying variance. There's got to be a cleaner way to do this, but the
  # approach here is to have a "magnifier" that "walks" between a max and a
  # min threshold
  if(varying_variance == TRUE){
    
    vary <- 0
    start_vary <- 0
    max_vary <- 1
    dir <- "increasing"
    
    for(i in 2:nrow(df)){
      
      if(dir == "increasing"){
        next_val = vary[[i-1]] + runif(1, -0.05, 0.15)
      } else {
        next_val = vary[[i-1]] - runif(1, -0.05, 0.15)
        if(next_val < 0) next_val <- 0
      }
      
      vary <- c(vary, next_val)
      
      if(next_val > max_vary) dir <- "decreasing"
      if(next_val <= 0) dir <- "increasing"
    }
    
    # Now apply that to the line
    df$value_vary <- vary
    df <- df |> 
      mutate(value = if_else(value > base, value * (1 + value_vary),
                             value * (1 - value_vary))) |> 
      mutate(value = if_else(value < 0, 0, value)) |> 
      select(-value_vary)
    
  }
  
  # Add the seasonality. This could get a lot more involved, but, for now, will
  # just doing a "typical" weekend dip
  if(increment == "day" & week_season == TRUE){
    df <- df |> 
      mutate(value = case_when(
        weekdays(date) == "Sunday" ~ value * 0.1,
        weekdays(date) == "Monday" ~ value * 0.87,
        weekdays(date) == "Tuesday" ~ value * 0.93,
        weekdays(date) == "Wednesday" ~ value,
        weekdays(date) == "Thursday" ~ value * 0.93,
        weekdays(date) == "Friday" ~ value * 0.87,
        TRUE ~ value * 0.1
      ))
  }
  
  # Add the trending element. If it's 0, we want it to be flat. Otherwise
  # it's the % increase/decrease between the base (the starting point) and the
  # end point
  trend_end <- base * (1 + trend) - base
  trend_multiples <- seq(0, trend_end, length.out = length(dates)) 
  df$value = df$value + trend_multiples
  
  # Add the intervention effect. This requires both an intervention date and
  # an effect of the intervention, which is the % increase or decrease from
  # the base value (adjusted by all of the previous criteria)
  if(!is.na(intervention_date) & !is.na(intervention_effect)){
    df <- df |> 
      mutate(value = if_else(date > intervention_date,
                             value * (1 + intervention_effect),
                             value))
  }
  
  df
  
}

