library(tidyverse)
library(rvest)

result <- data.frame(matrix(ncol = 7))

colnames(result) <- c("team", "wins", "games", "dp", "def_eff", "runs", "year")

for (year in 1876:2021) {
  html <- read_html(sprintf("https://www.baseball-reference.com/leagues/MLB/%s.shtml", year))
  
  w <- html %>% 
    html_node("#all_teams_standard_pitching") %>% 
    html_nodes(xpath = 'comment()') %>%
    html_text() %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    select(Tm, W)
  
  dp <- html %>% 
    html_nodes("#all_teams_standard_fielding") %>% 
    html_nodes(xpath = 'comment()') %>%
    html_text() %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    select(Tm, G, DP, DefEff, 'RA/G')
  
  tab <- left_join(w, dp, by = "Tm") %>% 
    filter(str_length(Tm) == 3) 
  
  tab[, 2:6] <- sapply(tab[, 2:6], as.numeric)
  
  colnames(tab) <- c("team", "wins", "games", "dp",  "def_eff", "runs")
  
  tab$year <- year
  
  result <- rbind(result, tab)
}

result$dp_game <- result$dp / result$games
  
tab %>% 
  ggplot() +
  geom_point(aes(x = dp, y = runs, color = wins)) +
  xlim(0, 70) + ylim(0, 6)

plot1 <- result %>% 
  ggplot() +
  geom_point(aes(x = dp_game, y = wins), color = "dodgerblue4", alpha = 0.5) +
  labs(x = "Double Plays per Game", y = "Wins", caption = "https://www.baseball-reference.com") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

ggsave("dpgw.png", plot1, width = 8, height = 6, units = "in")

dpw_lm <- lm(wins ~ dp_game, result)

summary(dpw_lm)

plot <- result %>% 
  ggplot() +
  geom_point(aes(x = year, y = dp_game), color = "dodgerblue4", alpha = 0.5) +
  geom_smooth(aes(x = year, y = dp_game), method = "loess", color = "red", alpha = 0.5) +
  labs(y = "Double Plays per Game", x = "Year", caption = "https://www.baseball-reference.com") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))
ggsave("dpgame.png", plot, width = 8, height = 6, units = "in")
