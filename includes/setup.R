
set.seed(61705)

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               scales,
               lubridate,
               showtext      # For using a custom font
               )

# See https://r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
# The first argument can be changed to any Google font. The second argument
# should not be changed.
font_add_google("Montserrat", family = "s_font")
showtext_auto()


# Config settings for styling
s_bgrnd <- "transparent"
s_labels <- "gray80"
s_line_1 <- "red"
s_line_2 <- "blue"

# The main theme
theme_main <- theme_minimal() +
  theme(plot.title.position = "plot",
        plot.background = element_rect(fill = s_bgrnd, color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray60"),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "s_font", size = 13, colour = s_labels),
        axis.line.x = element_line(color = s_labels),
        axis.title.y = element_text(family = "s_font", size = 14, colour = s_labels),
        axis.text.y = element_text(family = "s_font", size = 13, colour = s_labels),
        axis.line.y = element_blank()
  )

# Function to generate time-series with different characteristics
get_ts <- function(start = Sys.Date()-90, 
                   end = Sys.Date() - 1,
                   increment = "day",
                   week_season = FALSE,   # Only if increment is "day"; gives weekend dips
                   base = 10000, # base/starting number
                   trend = 0,    # The % change between the base and the final value
                   noise_level = 0.1,  # 0 = no noise; 1 = max noise
                   intervention_date = NA,
                   intervention_effect = NA # The % increase/decrease from the base expressed as a decimal
){
  
  # A bit of a hack, but a base of 0 is messy, so, if it's set to 0, make it close to 0
  if(base == 0) base <- 0.01
  
  # Set up the basic data frame
  dates = seq.Date(start, end, increment)
  base_values = runif(length(dates),
                 base * (1 - noise_level),
                 base * (1 + noise_level)) |> 
    round()
  
  df <- data.frame(date = dates,
                   value = base_values)
  
  # Add the seasonality. This could get a lot more involved, but, for now, will
  # just:
  #   - Make Saturday and Sunday 10% of the base value
  #   - Make Monday and Friday 80% of the base value
  #   - Make Tuesday and Thursday 90% of the base value
  if(increment == "day" & week_season == TRUE){
    df <- df |> 
      mutate(value = case_when(
        weekdays(date) == "Sunday" ~ value * 0.1,
        weekdays(date) == "Monday" ~ value * 0.8,
        weekdays(date) == "Tuesday" ~ value * 0.9,
        weekdays(date) == "Wednesday" ~ value,
        weekdays(date) == "Thursday" ~ value * 0.8,
        weekdays(date) == "Friday" ~ value * 0.7,
        TRUE ~ value * 0.1
      ))
  }
  
  # Add the trending element. If it's 0, we want it to be flat. Otherwise
  # it's the % increase/decrease between the base (the starting point) and the
  # end point
  trend_end <- base * (1 + trend)
  trend_multiples <- seq(base, trend_end, length.out = length(dates)) / base
  df$value = df$value * trend_multiples
  
  # Add the intervention effect. This requires both an intervention date and
  # an effect of the intervention, which is the % increase or decrease from
  # the base value (adjusted by all of the previous criteria)
  if(!is.na(intervention_date) & !is.na(intervention_effect)){
    df <- df |> 
      mutate(value = if_else(date > as.Date(intervention_date),
                             value * (1 + intervention_effect),
                             value))
  }
  
  df
  
}

