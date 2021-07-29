library(tidyverse)
library(rvest)
library(rjson)
library(RColorBrewer)
library(viridis)
library(lubridate)

html <- read_html("https://baseballsavant.mlb.com/savant-player/carlos-rodon-607074?stats=gamelogs-r-pitching-statcast&season=2021&gamelogs_event=&gamelogs_direction=&gamelogs_pitchtypes=&gamelogs_view=statcastGameLogs")

html %>% html_table()

text <- read.delim("rondon.txt")

fromJSON(file = "rondon.txt")

cols <- trimws(sapply(str_split(text[1:28 , 1], ":"),'[[',1))

savant_df <- data.frame(matrix(ncol = 28))

colnames(savant_df) <- cols

for (i in seq(0, 8609, 30)) {
  row <- trimws(sapply(str_split(text[(i + 1):(i + 28), 1], ":"), '[[', 2))
  savant_df <- rbind(savant_df, row)
}

savant_df <- savant_df %>% 
  filter(!is.na(play_id))

sum <- savant_df %>% 
  group_by(pn, event) %>% 
  summarize(n = n())

str_remove(sum, ",")

sum %>% 
  pivot_longer(cols = c("event", "pn"), names_to = "pn", values_to = "n")

sum %>% 
  ggplot() +
  geom_bar(aes(x = event, y = n, fill = pn), stat = "identity")

sum_hit <- sum %>% 
  filter(event %in% c("double,", "single,", "walk,", "home_run,"))

sum_out <- sum %>% 
  filter(event %in% c("field_out,", "force_out,", "grounded_into_double_play,", "strikeout,"))

sum_hit$event <- factor(sum_hit$event, levels = c("walk,", "single,", "double,", "home_run,"))

sum_hit$pn <- str_remove(sum_hit$pn, ",")

hit_d_plot <- sum_hit %>% 
  ggplot() +
  geom_bar(aes(x = event, y = n, fill = pn), stat = "identity") +
  scale_x_discrete(labels = c("Walk", "Single", "Double", "Home Run")) +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.8),
    legend.text = element_text(size = 15),
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    y = "Count",
    caption = "baseballsavant.mlb.com")

ggsave("rondon.png", hit_d_plot, width = 8, height = 6, units = "in")

savant_df$gd <- ymd(savant_df$gd)

savant_df$v <- as.numeric(savant_df$v)

savant_df$event <- str_remove(savant_df$event, ",")

date_e_plot <- savant_df %>% 
  group_by(gd, event) %>% 
  summarize(n = n()) %>% 
  filter(event %in% c("walk", "single", "double", "home_run", "strikeout")) %>% 
  ggplot() +
  geom_bar(aes(x = as.factor(gd), y = n, fill = event), stat = "identity") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.text = element_text(size = 15),
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "",
    y = "Count",
    caption = "baseballsavant.mlb.com")
ggsave("dateevent.png", date_e_plot, width = 11, height = 6, units = "in")

savant_df$pn <- str_remove(savant_df$pn, ",")

date_p_plot <- savant_df %>% 
  group_by(gd, pn) %>% 
  summarize(n = n()) %>% 
  filter(pn != "null") %>% 
  ggplot() +
  geom_bar(aes(x = as.factor(gd), y = n, fill = pn), stat = "identity") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.86, 0.94),
    legend.text = element_text(size = 15),
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "",
    y = "Count",
    caption = "baseballsavant.mlb.com")

ggsave("datepitch.png", date_p_plot, width = 12, height = 6, units = "in")
