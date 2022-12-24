
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
s_bgrnd <- "yellow"
s_line_1 <- "red"
s_line_2 <- "blue"

# The main theme
theme_main <- theme_minimal() +
  theme(plot.title.position = "plot",
        plot.background = element_rect(fill = s_bgrnd),
        panel.background = element_rect(fill = s_bgrnd),
        panel.border = element_rect(fill = NA, colour = NULL),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "s_font", size = 13, colour = "gray30"),
        axis.line.x = element_line(color = "gray20"),
        axis.title.y = element_text(family = "s_font", size = 14, colour = "gray30"),
        axis.text.y = element_text(family = "s_font", size = 13, colour = "gray30"),
        axis.line.y = element_blank()
  )

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

