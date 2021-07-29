library(tidyverse)
library(ggrepel)

swing_take <- read.csv("swing-take.csv")

batting <- read.csv("batting2021.csv")

merged <- left_join(swing_take, batting, by = "player_id") %>% 
  filter(!is.na(xba))

swing_take %>% 
  ggplot() +
  geom_histogram(aes(runs_all))

swing_take_svd <- svd(swing_take[, 7:12])

swing_take$x <- swing_take_svd$u[, 1]

swing_take$y <- swing_take_svd$u[, 2]

mariners_id <- swing_take$team_id[swing_take$last_name == "Haniger"]


ggplot() +
  geom_point(data = swing_take, aes(x = x, y = y, color = runs_all)) +
  geom_text(data = subset(swing_take, team_id == mariners_id), 
            aes(x = x, y = y, label = last_name))

merged_svd <- svd(merged[, c(7:12, 16:23)])

merged$x <- merged_svd$u[, 1]

merged$y <- merged_svd$u[, 2]

ggplot(merged, aes(x = x, y = y, label = ifelse(team_id == mariners_id, last_name.x, ""))) +
  geom_point() +
  geom_text_repel(box.padding = 1, max.overlaps = Inf, color = "gray45", family = "mono") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

plot <- ggplot(merged, aes(x = xba, y = xslg, color = , label = ifelse(team_id == mariners_id, last_name.x, ""))) +
  geom_point() +
  geom_text_repel(box.padding = 1, max.overlaps = Inf, color = "gray45", family = "mono") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

ggsave("kelenicbruh.png", plot)
