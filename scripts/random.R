library(rvest)
library(hrbrthemes)
library(tidyverse)
library(viridis)

html <- read_html("https://www.baseball-reference.com/leagues/MLB/2021-standard-pitching.shtml")

table <-
  html %>% html_nodes('#all_players_standard_pitching') %>%     # select node with comment
  html_nodes(xpath = 'comment()') %>%   # select comments within node
  html_text() %>%                       # return contents as text
  read_html() %>%                       # parse text as html
  html_node('table') %>%                # select table node
  html_table()                          # parse table and return data.frame

table[, 6:35] <- sapply(table[, 6:35], as.numeric)

table %>% 
  filter(BF < 80) %>% 
  ggplot() +
  geom_point(aes(x = as.numeric(BF), y = as.numeric(SO))) +
  scale_x_continuous() +
  scale_y_continuous()

player_list <- html %>% 
  html_nodes('#all_players_standard_pitching') %>%     # select node with comment
  html_nodes(xpath = 'comment()') %>%   # select comments within node
  html_text() %>% read_html() %>%  html_nodes("a") %>% html_attr("href")

player_list <- Filter(function(x) !any(grepl("team", x)), player_list)

result <- data.frame()

for (player in player_list) {
  player <- str_split(player, "/")[[1]][4]
  
  player <- str_split(player, "\\.")[[1]][1]
  
  url <- sprintf("https://www.baseball-reference.com/players/gl.fcgi?id=%s&t=p&year=2021#pitching_gamelogs", player)
  
  table <- read_html(url) %>% 
    html_table() %>% 
    data.frame()
  
  print(table)
  
  table <- table %>% 
    mutate(player = player)
  
  result <- rbind(result, table)
}

read_html("https://www.baseball-reference.com/players/gl.fcgi?id=smithjo05&t=p&year=2021#pitching_gamelogs") %>% 
  html_nodes('#pitching_gamelogs') %>% 
  html_text() 

result[, 11:50] <- sapply(result[, 11:50], as.numeric)

result <- result %>% 
  filter(!is.na(BF))

result %>% 
  filter(BF < 20) %>% 
  ggplot() +
  geom_point(aes(x = BF, y = SO))

result_clean <- result %>% 
  filter(BF < 20)
  
result_clean %>% 
  ggplot() +
  geom_tile(aes(x = BF, y = SO, fill = ERA))

test %>% 
  ggplot() +
  geom_point(aes(x = as.numeric(BF), y = as.numeric(SO)))

test <- result %>% filter(SO != "SO" & Date != "")

test %>% 
  ggplot() +
  geom_tile(aes(x = as.numeric(BF), y = as.numeric(SO), fill = as.numeric(ERA)))

test[, 11:50] <- sapply(test[, 11:50], as.numeric)

test %>% 
  group_by(BF, SO) %>% 
  summarize(IP = mean(IP, na.rm = T)) %>% 
  ggplot() +
  geom_raster(aes(x = BF, y = SO, fill = (IP))) +
  scale_fill_gradient(low="white", high="blue") +
  labs(fill = "IP") +
  theme_ipsum()

test %>% 
  group_by(BF, SO) %>% 
  summarize(IP = mean(IP, na.rm = T)) %>% 
  ggplot() +
  geom_raster(aes(x = BF, y = SO, fill = IP)) +
  scale_fill_gradient(low="white", high="blue") +
  labs(fill = "IP",
       caption = "https://www.baseball-reference.com/") +
  theme_ipsum()

test %>% 
  group_by(BF, SO) %>% 
  summarize(ERA = mean(ERA, na.rm = T)) %>% 
  ggplot() +
  geom_raster(aes(x = BF, y = SO, fill = log(ERA))) +
  scale_fill_viridis(discrete=FALSE) +
  labs(fill = "ERA",
       caption = "https://www.baseball-reference.com/") +
  theme_ipsum()

test %>% 
  group_by(BF, SO) %>% 
  summarize(WPA = mean(WPA, na.rm = T)) %>% 
  ggplot() +
  geom_raster(aes(x = BF, y = SO, fill = WPA)) +
  scale_fill_gradient(low="white", high="blue") +
  labs(fill = "WPA",
       caption = "https://www.baseball-reference.com/") +
  theme_ipsum()

plot <- test %>% 
  group_by(BF, SO) %>% 
  summarize(ER = mean(ER, na.rm = T)) %>% 
  ggplot() +
  geom_raster(aes(x = BF, y = SO, fill = log(ER))) +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "Batters Faced",
    y = "Strikeouts",
    fill = "ER",
    caption = "https://www.baseball-reference.com/")

ggsave("sobf.png", plot, width = 8, height = 6, units = "in")

test %>% 
  group_by(SO, ER) %>% 
  summarize(n = n(), ip = mean(IP)) %>% 
  ggplot() +
  geom_point(aes(x = SO, y = ER, size = n, color = ip))

plot2 <- test %>% 
  group_by(SO, ER) %>% 
  summarize(n = n(), ip = mean(IP)) %>% 
  ggplot() +
  geom_tile(aes(x = SO, y = ER, fill = ip)) +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "Strikeouts",
    y = "Earned Runs",
    fill = "IP",
    caption = "https://www.baseball-reference.com/")

ggsave("soer.png", plot2, width = 8, height = 6, units = "in")

test %>% 
  group_by(BB, ER) %>% 
  summarize(n = n(), IP = mean(IP)) %>% 
  ggplot() +
  geom_tile(aes(x = BB, y = ER, fill = IP)) +
  scale_fill_viridis(discrete=FALSE) +
  theme_minimal() +
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "Bases on balls",
    y = "Earned Runs",
    fill = "IP",
    caption = "https://www.baseball-reference.com/")

test %>% 
  group_by(Pit, ER) %>% 
  summarize(n = n(), SO = mean(SO)) %>% 
  ggplot() +
  geom_point(aes(x = Pit, y = ER, size = SO)) +
  scale_fill_viridis(discrete=FALSE)

test %>% 
  group_by(SO, ER) %>% 
  summarize(Pit = mean(Pit)) %>% 
  ggplot() +
  geom_tile(aes(x = SO, y = ER, fill = Pit)) +
  scale_fill_viridis(discrete=FALSE)

test %>% 
  group_by(ERA, HR) %>% 
  summarize(n = n()) %>% 
  ggplot() +
  geom_tile(aes(x = log(ERA), y = HR, fill = n))

test <- result %>% filter(!is.na(as.numeric(Rk)))

averages <- test %>% 
  filter(!is.infinite(ERA)) %>% 
  group_by(as.numeric(Rk)) %>%
  summarize(ERA = mean(ERA, na.rm = T))

colnames(averages) <- c("Rk", "ERA")

ggplot() +
  geom_boxplot(data = test, aes(x = as.factor(as.numeric(Rk)), y = ERA), alpha = 0.5) +
  geom_line(data = averages, aes(x = as.factor(Rk), y = ERA), group = 1, color = "red") +
  labs(x = "Game #", caption = "www.baseball-reference.com") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

plot <- ggplot() +
  geom_boxplot(data = test, aes(x = as.factor(as.numeric(Rk)), y = ERA), alpha = 0.5) +
  labs(x = "Game #", caption = "www.baseball-reference.com") +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

ggsave("eraavg.png", plot, width = 10, height = 6, units = "in")
