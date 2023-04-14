library(baseballr)
library(tidyverse)
library(kableExtra)
library(randomForest)

player_ids <- read.csv("https://www.smartfantasybaseball.com/PLAYERIDMAPCSV")

paul <- scrape_statcast_savant(
  start_date = "2021-04-01",
  end_date = Sys.Date(),
  playerid = as.numeric(player_ids$MLBID[player_ids$PLAYERNAME == "Paul Sewald"]),
  player_type = "pitcher"
)

paulplot <- paul %>% 
  group_by(pitch_name) %>% 
  summarize(x = mean(plate_x, na.rm = T), 
            z = mean(plate_z, na.rm = T), 
            velo = mean(release_speed), 
            rpm = mean(release_spin_rate),
            strike_pct = mean(description %in% c("called_strike", "swinging_strike")),
            n = n()) %>% 
  ggplot() +
  geom_point(data = paul, (aes(x = plate_x, y = plate_z, color = pitch_name)), alpha = 0.1) +
  geom_point(aes(x = x, y = z, color = pitch_name)) +
  labs(x = "X", y = "Z", caption = "@swingmisstake | baseballr") +
  facet_wrap(~ pitch_name) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        legend.position = "none")

ggsave("../vis/paulplace.png", paulplot)

paul$sept <- ifelse(paul$game_date >= as.Date("2021-09-01"), "September", "Pre-September")

paul %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = pitch_name)) +
  facet_wrap(~ events)

paul %>% 
  group_by(sept, pitch_name) %>% 
  summarize(x = mean(plate_x, na.rm = T), y = mean(plate_z, na.rm = T)) 

paul_dense <- paul %>% 
  filter(pitch_name != c("Changeup", NA)) %>% 
  mutate(slump = paste(sept, pitch_name)) %>% 
  ggplot() +
  geom_density_2d_filled(aes(x = plate_x, y = plate_z)) +
  facet_wrap(~ slump) +
  labs(x = "x", y = "z", caption = "@swingmisstake | baseballr", title = "Paul Sewald's September Struggles") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        legend.position = "none")
ggsave("~/desktop/projects/mlb/vis/pauldense2.png", paul_dense, width = 13, height = 8, units = "in")

paul %>% 
  filter(pitch_name != c("Changeup", NA)) %>% 
  mutate(slump = paste(sept, pitch_name)) %>% 
  group_by(slump) %>% 
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T))

paul_tab <- paul %>% 
  filter(pitch_name != c("Changeup", NA)) %>% 
  group_by(sept, pitch_name) %>% 
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T)) %>% 
  pivot_wider(id_cols = sept, names_from = pitch_name, values_from = xba)

paul_tab %>% 
  kable(col.names = c("", "4-Seam Fastball", "Slider")) %>% 
  kable_styling() %>% 
  row_spec(0:2, monospace = TRUE) %>%
  column_spec(1, bold = TRUE) %>% 
  column_spec(2, 
              color = "white",
              background = spec_color(option = "D", paul_tab$`4-Seam Fastball`, end = 0.7, direction = -1)) %>% 
  column_spec(3, 
              color = "white",
              background = spec_color(option = "D", paul_tab$Slider, end = 0.7, direction = -1))

paul$hr <- paul$events == "home_run"

paul_clean <- paul %>% 
  filter(!is.na(release_spin_rate)) %>% 
  select(hr, release_speed, release_pos_x, release_pos_y, release_pos_z, release_spin_rate)

paul_clean[is.na(paul_clean)] <- FALSE

randomForest(hr ~ release_speed + release_pos_x + release_pos_y + release_pos_z + release_spin_rate, data = paul_clean)

lm(hr ~ release_speed + release_pos_x + release_pos_y + release_pos_z + release_spin_rate, data = paul_clean) %>% summary()
