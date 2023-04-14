library(baseballr)
library(tidyverse)
library(viridis)

theme_cons <- theme_minimal() +
  theme(text = element_text(family = "Titillium Web"))

topKzone = 3.5
botKzone = 1.6
inKzone = -.95
outKzone = 0.95
kZone = data.frame(
  x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
  , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

julio <- scrape_statcast_savant_batter(start_date = "2022-03-01", end_date = "2023-03-01", 677594)

julio_all <- julio

julio <- julio %>% 
  filter(game_date >= "2022-04-08")

julio %>%
  filter(description %in% c("called_strike", "swinging_strike")) %>% 
  mutate(is_so = events == "strikeout", is_so = replace_na(is_so, FALSE), strikes = paste0(strikes, " strike(s)"), outzone = plate_x < -0.95 | plate_x > 0.95 | plate_z < 1.6 | plate_z > 3.5) %>% 
  ggplot() +
  geom_rect(data = kZone, aes(xmin = -0.95, xmax = 0.95, ymin = 1.6, ymax = 3.5), alpha = 0.05) + 
  geom_point(aes(x = plate_x, y = plate_z, color = outzone, shape = description, alpha = outzone), size = 4) +
  scale_color_manual(name = "", labels = c("Strike Looking (in the zone)", "Strike Swinging (outside of the zone)", "Strike Looking (outside of the zone)", "Strike Swinging (outside of the zone"), values = c("dodgerblue", "red", "dodgerblue", "red")) +
  scale_shape_manual(name = "", labels = c("Strike Looking (in the zone)", "Strike Swinging (outside of the zone)", "Strike Looking (outside of the zone)", "Strike Swinging (outside of the zone"), values = c(1, 4, 1, 4)) +
  scale_alpha_manual(name = "", labels = c("Strike Looking (in the zone)", "Strike Swinging (outside of the zone)", "Strike Looking (outside of the zone)", "Strike Swinging (outside of the zone"), values = c(0.4, 1, 0.4, 1)) +
  xlim(-2, 2) +
  ylim(0.6, 4.5) +
  facet_wrap(~ strikes) +
  labs(x = "Catcher's Perspective", y = "", title = "Julio's Bad Luck On 2 Strike Pitches", subtitle = "Red - Out of zone, Blue - In zone, o - No Swing, x - swing.", caption = "@swingmisstake | Source: baseballsavant (baseballr)") +
  guides(alpha = "none") +
  theme_cons +
  theme(strip.text = element_text(size = 16, margin = margin(1, 0, 1, 0, unit = "cm")),
        axis.title.x = element_text(size = 14, margin = margin(1, 0, 1, 0, unit = "cm")),
        legend.position = c(0.1, 0.9),
        plot.title = element_text(size = 22, hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, face = "italic"))

ggsave("../vis/juliofd.png", width = 12, height = 8, units = "in")

julio %>% 
  ggplot() +
  geom_density_2d_filled(aes(x = plate_x, y = plate_z))

julio %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z)) +
  geom_path(aes(x, y), data = kZone, linetype = "dashed") + 
  theme_cons
  

julio %>% 
  group_by(game_date) %>% 
  summarize(contact = mean(!is.na(estimated_ba_using_speedangle)), xba = mean(estimated_ba_using_speedangle, na.rm = T)) %>% 
  ggplot() +
  geom_bar(aes(x = game_date, y = contact), stat = "identity", fill = "#005c5c") +
  theme_cons
