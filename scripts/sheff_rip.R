library(tidyverse)
library(rvest)
library(kableExtra)

html <- read_html("https://baseballsavant.mlb.com/savant-player/justus-sheffield-656954?stats=gamelogs-r-zones-statcast&season=2021")

html %>% html_nodes("#gamelogs_statcast") %>% html_text()

run_pitch <- read_html("https://baseballsavant.mlb.com/savant-player/justus-sheffield-656954?stats=statcast-r-pitching-mlb") %>% 
  html_table() %>% 
  .[[29]]

run_pitch_val <- run_pitch[, c(1,2,5)]

colnames(run_pitch_val) <- c("year", "pitch", "run_value")

rpv_wide <- pivot_wider(data = run_pitch_val, names_from = pitch, values_from = run_value)

colnames(rpv_wide) <- c("year", "4-Seamer", "Changeup", "Slider", "Sinker", "Curveball")

rpv_wide <- rpv_wide %>% 
  filter(year >= 2020)

rpv_wide <- rpv_wide[order(rpv_wide$year), ]

colnames(rpv_wide) <- c("", "4-Seamer", "Changeup", "Slider", "Sinker", "Curveball")

rpv_wide[, 1:5] %>% 
  kable(caption = "Sheffield's Run-Value by Pitch 2020 vs. 2021") %>% 
  kable_styling() %>% 
  column_spec(1, underline = TRUE) %>% 
  row_spec(1:2, monospace = TRUE) %>% 
  column_spec(2, 
              color = "white",
              background = spec_color(rpv_wide$`4-Seamer`, begin = 0.01, end = 0.6, direction = -1, option = "viridis"), 
              popover = paste("am:", rpv_wide$`4-Seamer`)) %>% 
  column_spec(3, 
              color = "white",
              background = spec_color(rpv_wide$Changeup, begin = 0.01, end = 0.6, direction = -1, option = "viridis"), 
              popover = paste("am:", rpv_wide$Changeup)) %>% 
  column_spec(4, 
              color = "white",
              background = spec_color(rpv_wide$Slider, begin = 0.01, end = 0.6, direction = -1, option = "viridis"), 
              popover = paste("am:", rpv_wide$Slider)) %>% 
  column_spec(5, 
              color = "white",
              background = spec_color(rpv_wide$Sinker, begin = 0.01, end = 0.6, direction = -1, option = "viridis"), 
              popover = paste("am:", rpv_wide$Sinker))


column_spec(2, color = "white", background = spec_color(rpv_wide[1:2, 2], end = 0.7, direction = -1), popover = paste("am:", rpv_wide[1:2, 2])) %>% 
  column_spec(3, color = "white", background = spec_color(rpv_wide[1:2, 3], end = 0.7, direction = -1), popover = paste("am:", rpv_wide[1:2, 3])) %>% 
  column_spec(4, color = "white", background = spec_color(rpv_wide[1:2, 4], end = 0.7, direction = -1), popover = paste("am:", rpv_wide[1:2, 4])) %>% 
  column_spec(5, color = "white", background = spec_color(rpv_wide[1:2, 5], end = 0.7, direction = -1), popover = paste("am:", rpv_wide[1:2, 5])) %>% 
