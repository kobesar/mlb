library(tidyverse)
library(rvest)

result <- data.frame(matrix(ncol = 7))

colnames(result) <- c("team", "wins", "wpct", "so", "games", "po", "year")

for (year in 1876:2021) {
  html <- read_html(sprintf("https://www.baseball-reference.com/leagues/MLB/%s.shtml", year))
  
  w <- html %>% 
    html_node("#all_teams_standard_pitching") %>% 
    html_nodes(xpath = 'comment()') %>%
    html_text() %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    select(Tm, W, 'W-L%', SO) %>% 
    head(-2)
  
  po <- html %>% 
    html_nodes("#all_teams_standard_fielding") %>% 
    html_nodes(xpath = 'comment()') %>%
    html_text() %>% 
    read_html() %>% 
    html_node("table") %>% 
    html_table() %>% 
    select(Tm, G, PO) %>% 
    head(-2)
  
  print(w)
  print(po)
  
  tab <- left_join(w, po, by = "Tm")
  
  tab[, 2:6] <- sapply(tab[, 2:6], as.numeric)
  
  colnames(tab) <- c("team", "wins", "wpct", "so", "games", "po")
  
  tab$year <- year
  
  result <- rbind(result, tab)
}

result <- result %>% 
  filter(!is.na(so) & !is.na(po))

result %>% 
  ggplot() +
  geom_point(aes(x = so, wpct))

result %>% 
  ggplot() +
  geom_point(aes(x = po, wpct))

result %>% 
  ggplot() +
  geom_point(aes(x = so, y = po))

result$so_game <- result$so / result$games

result$po_game <- result$po / result$games

result %>% 
  ggplot() +
  geom_point(aes(x = po_game, y = wpct))

result %>% 
  ggplot() +
  geom_point(aes(x = so_game, y = wpct), alpha = 0.5)

plot <- result %>% 
  ggplot() +
  geom_point(aes(x = year, y = so_game, color = wpct)) +
  scale_color_gradient(low = "lightcoral",
                       high = "dodgerblue") + 
  labs(x = "Year", y = "SO/G", caption = "https://www.baseball-reference.com") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))

ggsave("sog.png", plot, width = 10, height = 6)

plot <- result %>% 
  ggplot() +
  geom_point(aes(x = year, y = wpct), color = "dodgerblue4", alpha = 0.5) +
  labs(x = "Year", y = "Win Percentage", caption = "https://www.baseball-reference.com") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F9F9F9", color = "white"))
ggsave("distwinp.png", plot, width = 10, height = 6)  
