library(baseballr)
library(tidyverse)
library(rvest)

player_ids <- read.csv("https://www.smartfantasybaseball.com/PLAYERIDMAPCSV")

html <- read_html("https://www.espn.com/mlb/team/roster/_/name/sea/seattle-mariners")

pitchers <- html %>% 
  html_table() %>% 
  .[[1]]

pitchers$Name <- str_remove_all(pitchers$Name, "[0-9]")

pitchers$first <- unlist(lapply(str_split(pitchers$Name, " "), `[[`, 1))

pitchers$last <- unlist(lapply(str_split(pitchers$Name, " "), `[[`, 2))

pitchers$id <- player_ids %>% 
  filter(FIRSTNAME == pitchers$first & LASTNAME == pitchers$last) %>% 
  select(MLBID)

kelnic <- scrape_statcast_savant(
  start_date = "2021-04-01",
  end_date = Sys.Date(),
  playerid = 672284,
  player_type = "batter"
)

torrens <- scrape_statcast_savant(
  start_date = as.Date("2021-04-01"),
  end_date = Sys.Date(),
  playerid = 620443,
  player_type = "batter"
)

batters2021 <- scrape_statcast_savant_batter_all(
  start_date = as.Date("2021-04-01"),
  end_date = Sys.Date(),
)

pitchers2021 <- scrape_statcast_savant_pitcher_all(
  start_date = as.Date("2021-04-01"),
  end_date = Sys.Date(),
)

viz_gb_on_period(as.Date("2021-04-01"), as.Date("2021-08-15"), "AL West")

ggspraychart(kelnic, x_value = "hc_x", y_value = "-hc_y", fill_value = "events")

kelnic %>% 
  ggplot() +
  geom_point(aes(x = pfx_x, y = pfx_z, size = estimated_ba_using_speedangle)) +
  facet_wrap(~ strikes)

kelnic %>% 
  # group_by(plate_x, plate_z) %>% 
  # summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T)) %>% 
  ggplot(aes(x = plate_x, y = plate_z)) +
  geom_hex() +
  theme_minimal()

kelnic$call_down <- ifelse(kelnic$game_date <= "2021-06-05", "before", "after")

kelnic$call_down <- factor(kelnic$call_down, levels = c("before", "after"))

torrens$call_down <- ifelse(torrens$game_date <= "2021-06-15", "before", "after")

torrens$call_down <- factor(torrens$call_down, levels = c("before", "after"))

plot <- kelnic %>% 
  filter(type != "X") %>% 
  ggplot() +
  geom_point(aes(x = pfx_x, y = pfx_z, color = type)) +
  scale_color_manual(name = "", values = c("dodgerblue", "firebrick2"), labels = c("Ball", "Strike")) +
  labs(x = "x", y = "y", title = "Kelenic's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

ggsave("kelenicszone.png", plot)

kelnic %>% 
  filter(type != "X") %>% 
  ggplot() +
  geom_point(aes(x = pfx_x, y = pfx_z, color = type)) +
  facet_wrap(~ pitch_type)

kelnic %>% 
  filter(type != c("X", "S")) %>% 
  ggplot() +
  geom_bin2d(aes(x = pfx_x, y = pfx_z)) +
  labs(x = "x", y = "y", title = "Kelenic's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

kelnic %>% 
  filter(type != c("X", "B")) %>% 
  ggplot() +
  geom_bin2d(aes(x = pfx_x, y = pfx_z)) +
  labs(x = "x", y = "y", title = "Kelenic's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

torrens %>% 
  filter(type != "X") %>% 
  ggplot() +
  stat_density2d(aes(x = pfx_x, y = pfx_z, color = type)) +
  labs(x = "x", y = "y", title = "Torrens's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

torrens %>% 
  filter(events == "home_run") %>% 
  ggplot() +
  geom_point(aes(x = pfx_x, y = pfx_z, color = pitch_type, size = launch_angle)) +
  labs(x = "x", y = "y", title = "Torrens's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

torrens$hard_hit <- ifelse(torrens$launch_speed > 95, TRUE, FALSE)

torrens$dist_center <- sqrt(torrens$plate_x^2 + torrens$plate_z^2)

torrens %>% 
  filter(type == "X") %>% 
  ggplot() +
  geom_point(aes(x = pfx_x, y = pfx_z, color = hard_hit)) +
  labs(x = "x", y = "y", title = "Torrens's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

torrens %>% 
  filter(description %in% c("ball", "swinging_strike")) %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = description)) +
  labs(x = "x", y = "y", title = "Torrens's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)

kelnic %>% 
  filter(description %in% c("ball", "swinging_strike", "swinging_strike_blocked")) %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = type)) +
  labs(x = "x", y = "y", title = "Kelenic's Strikezone Before/After Call-down", caption = "@swingmisstake | baseballr") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  facet_wrap(~ call_down)


