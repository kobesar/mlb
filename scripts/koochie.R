library(baseballr)
library(tidyverse)
library(rvest)

player_ids <- read.csv("https://www.smartfantasybaseball.com/PLAYERIDMAPCSV")

koochie <- scrape_statcast_savant_pitcher(
  start_date = as.Date("2021-04-01"),
  end_date = Sys.Date(),
  pitcherid = as.numeric(player_ids$MLBID[player_ids$LASTNAME == "Kikuchi"])
)

plot <- koochie %>%  
  mutate(ass = (game_date == "2021-08-20")) %>% 
  group_by(ass, pitch_name) %>% 
  summarize(x = mean(release_pos_x), y = mean(release_pos_z)) %>% 
  ggplot(aes(x = x, y = y, color = pitch_name)) +
  geom_point(aes(shape = ass, size = 5)) +
  scale_shape_manual(values = c(4, 16)) +
  geom_line(alpha = 0.5) +
  guides(shape = FALSE, size = FALSE) +
  labs(color = "Pitch Type") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        legend.position = c(0.8, 0.8))

ggsave("../vis/koochie.png", plot)

plot <- koochie %>% 
  group_by(game_date) %>% 
  summarize(rpm = mean(release_spin_rate)) %>% 
  ggplot() +
  geom_line(aes(x = game_date, y = rpm), color = "dodgerblue4") +
  labs(x = "Date", y = "Spin Rate (rpm)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"))
ggsave("../vis/koochiespin.png", plot)

koochie %>% 
  mutate(ass = (game_date == "2021-08-20"), group = paste(pitch_name, ass, sep="-")) %>% 
  group_by(group) %>% 
  summarize(x = mean(plate_x), y = mean(plate_z)) %>% 
  ggplot() +
  geom_point(aes(x = x, y = y, color = group)) +
  geom_point(data = koochie, aes(x = plate_x, y = plate_z, color = pitch_name)) +
  facet_wrap(~ group) +
  theme_min()

plot <- koochie %>% 
  mutate(count = paste(balls, "-", strikes)) %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = pitch_name)) +
  facet_wrap(~ count) +
  theme_min() +
  theme(legend.position = "top") +
  labs(caption = "@swingmisstake | baseballr", color = "Pitch Type", x = "X", y = "Z")
ggsave("../vis/koochiecount.png", plot)

koochie %>% 
  group_by(game_date, at_bat_number) %>% 
  summarize(rpm = mean(release_spin_rate)) %>% 
  ggplot() +
  geom_point(aes(x = at_bat_number, y = rpm, color = game_date))

koochie %>% 
  group_by(at_bat_number) %>% 
  summarize(rpm = mean(release_spin_rate)) %>% 
  ggplot() +
  geom_point(aes(x = at_bat_number, y = rpm))

koochie %>% 
  group_by(at_bat_number) %>% 
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = at_bat_number, y = xba))

lm(estimated_ba_using_speedangle ~  plate_x + plate_z + stand, data = koochie)

theme_min <- function(){ 
  font <- "mono"   #assign font family up front
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      plot.title = element_text(face = "italic"),
      plot.caption = element_text(hjust = 1, color = "#696969"),
      text = element_text(family = "mono"),
      plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
      panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")
    )
  
}
