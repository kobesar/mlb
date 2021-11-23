library(baseballr)
library(tidyverse)
library(kableExtra)

playerid_lookup("Gonzalez", "Marco")

marco <- scrape_statcast_savant_pitcher(
  start_date = as.Date("2021-04-01"),
  end_date = Sys.Date(),
  pitcherid = 594835
)

marco %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z)) +
  
  facet_wrap(~ game_date)

marco %>% 
  group_by(game_date, pitch_name) %>%
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T), strikes = sum(description %in% c("called_strike", "swinging_strike"))) %>% 
  ggplot() +
  geom_line(aes(x = game_date, y = strikes, color = pitch_name))

monthtab <- marco %>% 
  mutate(month = strftime(game_date, format = "%m")) %>% 
  group_by(month, pitch_name) %>%
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T), strike_pc = round(sum(description %in% c("called_strike", "swinging_strike")) / length(description), 2), len = length(description)) %>% 
  select(month, pitch_name, strike_pc) %>% 
  pivot_wider(id_cols = month, names_from = pitch_name, values_from = strike_pc)

monthtab %>% 
  kable() %>% 
  kable_styling(html_font = "monospace") %>% 
  column_spec(2, 
               color = ifelse(monthtab[1:4, 2] != max(monthtab[1:4, 2]), "black", "white"),
               background = 
                ifelse(monthtab[1:4, 2] == max(monthtab[1:4, 2]), 
                       spec_color(monthtab[1:4, 2]$`4-Seam Fastball`, end = 0.7, direction = -1), 
                       "white"
              )) %>% 
  column_spec(3, 
              color = ifelse(monthtab[1:4, 3] != max(monthtab[1:4, 3]), "black", "white"),
              background = 
                ifelse(monthtab[1:4, 3] == max(monthtab[1:4, 3]), spec_color(monthtab[1:4, 3]$Changeup, end = 0.7, direction = -1),
                       "white")
  ) %>% 
  column_spec(4, 
                    color = ifelse(monthtab[1:4, 4] != max(monthtab[1:4, 4]), "black", "white"),
                    background = 
                ifelse(monthtab[1:4, 4] == max(monthtab[1:4, 4]), spec_color(monthtab[1:4, 4]$Curveball, end = 0.7, direction = -1),
                       "white")
  ) %>% 
  column_spec(5, 
                    color = ifelse(monthtab[1:4, 5] != max(monthtab[1:4, 5]), "black", "white"),
                    background = 
                ifelse(monthtab[1:4, 5] == max(monthtab[1:4, 5]), spec_color(monthtab[1:4, 5]$Cutter, end = 0.7, direction = -1),
                                        "white")
  ) %>% 
  column_spec(6, 
              color = ifelse(monthtab[1:4, 6] != max(monthtab[1:4, 6]), "black", "white"),
              background = 
                ifelse(monthtab[1:4, 6] == max(monthtab[1:4, 6]), spec_color(monthtab[1:4, 6]$Sinker, end = 0.7, direction = -1),
                       "white")
  ) 


marco$as_break <- ifelse(marco$game_date < as.Date("2021-07-16"), "before", "after")

marcoas <- marco %>% 
  group_by(as_break, pitch_name) %>% 
  summarize(xba = round(mean(estimated_ba_using_speedangle, na.rm = T), 2), strike_pc = round(sum(description %in% c("called_strike", "swinging_strike")) / length(description), 2), len = length(description)) %>% 
  select(as_break, pitch_name, xba) %>% 
  pivot_wider(id_cols = pitch_name, names_from = as_break, values_from = xba) %>% 
  mutate(diff = after - before)

marcoas <- marcoas[, c(1, 3, 2, 4)]

marcoas %>% 
  kable(col.names = c("", "Before", "After", "Difference")) %>% 
  kable_styling(html_font = "monospace") %>% 
  column_spec(4, color = "white",
              background = spec_color(marcoas$diff, end = 0.7, direction = -1)) 
