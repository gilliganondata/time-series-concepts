
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
                   seasonality = 7,
                   base = 10000, # base/starting number
                   trend = 0,    # The % change between the base and the final value
                   noise_level = 0.1,  # 0 = no noise; 1 = max noise
                   intervention_date = NA,
                   intervention_effect = NA # -1 to 1
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
                   values = base_values)
  
  # Add the seasonality
  
  # Add the trending element. If it's 0, we want it to be flat. Otherwise
  # it's the % increase/decrease between the base (the starting point) and the
  # end point
  trend_end <- base * (1 + trend)
  trend_multiples <- seq(base, trend_end, length.out = length(dates)) / base
  df$values = df$values * trend_multiples
  
  df
  
}

