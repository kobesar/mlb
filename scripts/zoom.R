library(tidyverse)
library(rvest)
library(ggborderline)
library(ggtext)
library(ggimage)
library(png)
library(patchwork)

url <- "https://www.baseball-reference.com/players/gl.fcgi?id=wongko01&t=b&year=%s"

tm_colors <- read.csv("https://raw.githubusercontent.com/camdenk/mlbplotR/main/data-raw/MLB_Colors_Logos.csv")

result <- data.frame(matrix(ncol=5))

colnames(result) <- names(table)

for (year in 2013:2023) {
  page <- read_html(sprintf(url, year))
  table <- page %>% 
    html_node("#batting_gamelogs") %>% 
    html_table() %>% 
    data.frame() %>% 
    mutate(Rk = as.numeric(Rk)) %>% 
    filter(!is.na(Rk)) %>% 
    mutate(year = year) %>% 
    select(year, Tm, Rk, H, BA)
  
  result <- rbind(result, table)
  Sys.sleep(1)
}

result <- result %>%
  mutate_at(3:5, as.numeric)

result <- result %>% 
  group_by(year) %>% 
  arrange(Rk) %>% 
  mutate(hits = cumsum(H)) 

result$year <- as.character(result$year)

result <- rbind(result, result %>% group_by(Rk) %>% summarize(hits = mean(hits), H = mean(H, na.rm = T), BA = mean(BA, na.rm = T)) %>% mutate(year = "avg") %>% relocate("year", .before = "Rk") %>% relocate("hits", .after = "BA"))

result <- result %>% 
  left_join(tm_colors, by = c("Tm" = "team_abbr"))

result <- result %>%
  mutate(label = paste0("<span style='margin-bottom: 50%;'>", hits, " Hit(s), ", year, "</span><img src='", team_logo_espn, "'  width='20'>"))

# result %>% 
#   ggplot() +
#   geom_borderstep(aes(x = Rk, y = BA, group = year, alpha = year == 2023)) +
#   geom_point(data = (result %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk, y = BA, alpha = year == 2023)) +
#   geom_text(data = (result %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk + 3, y = BA, alpha = year == 2023, label = year)) +
#   scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3)) +
#   labs(x = "Number of Games Into The Season", y = "BA", title = "Kolten Wong's BA Over The Season")

result %>% 
  filter(Rk < 8) %>%
  ggplot() +
  geom_borderstep(aes(x = Rk, y = hits, group = year, alpha = year %in% c(2023, "avg"), color = ifelse(is.na(team_color), "black", team_color), linetype = year == "avg")) +
  geom_point(data = (result %>% filter(Rk < 8 & year %in% c(2023, "avg")) %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk, y = hits, color = ifelse(is.na(team_color), "black", team_color))) +
  geom_text(data = (result %>% filter(Rk < 8 & year %in% c(2023, "avg")) %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk + 0.1, y = hits,label = paste0(hits, " Hit(s), ", ifelse(year == "avg", "Career Average", year)), color = ifelse(is.na(team_color), "black", team_color)), size = 3, family = "Source Sans Pro", hjust = 0) +
  geom_image(data = (result %>% filter(Rk < 8 & year %in% c(2023, "avg")) %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk+1.3, y = hits, image = team_logo_espn), size = 0.015, asp = 4/3) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_color_identity() +
  xlim(0, 20) +
  ylim(0, 15) +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 20),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.y.right = element_text(vjust = 0.986, angle = 0)
  )

p1 <- result %>% 
  ggplot() +
  geom_borderstep(aes(x = Rk, y = hits, group = year, alpha = year %in% c(2023, "avg"), color = ifelse(is.na(team_color), "black", team_color), linetype = year == "avg")) +
  geom_point(data = (result %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk, y = hits, color = ifelse(is.na(team_color), "black", team_color))) +
  geom_text(data = (result %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk+0.5, y = ifelse(year == 2017, 103, hits), label = paste0(hits, " Hit(s), ", year), color = ifelse(is.na(team_color), "black", team_color)), size = 3, family = "Source Sans Pro", hjust = 0) +
  geom_image(data = (result %>% group_by(year) %>% filter(Rk == max(Rk))), aes(x = Rk+10, y = ifelse(year == 2017, 103, hits), image = team_logo_espn), size = 0.015, asp = 4/3, hjust = 0) +
  geom_text(data = data.frame(x = 75, y = 65, label = "Career Average"), aes(x = x, y = y, label = label), family = "Source Sans Pro", angle = 30, fontface = "italic") +
  scale_y_continuous(position = "right") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.2)) +
  scale_color_identity() +
  labs(x = "Season Game Number Played", y = "Hits", title = "Kolten Wong Early Season Slump, When Will It End?", subtitle = "Currently, he is underperforming his career average, so it wouldn't be surpirising if either sucks like Winker did last year or picks it up and starts raking.", caption = "@swingmisstake | baseball-reference.com") +
  theme_minimal() +
  theme(
    text = element_text(family = "Source Sans Pro"),
    plot.title = element_text(size = 20, margin=margin(10,0,10,0)),
    plot.subtitle = element_text(margin=margin(10,0,30,0), size = 12, face = "italic"),
    legend.position = "none",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(size = 12),
    axis.text.y.right = element_text(face = "bold", size = 11),
    axis.title.y.right = element_text(vjust = 0.986, angle = 0, face = "bold", color=alpha('black', 0.8))
  )

img <- readPNG("~/Desktop/Projects/mlb/vis/zoom.png", native = TRUE)

p1 + inset_element(p = img,
                   left = 0.02,
                   bottom = 0.65,
                   right = 0.4,
                   top = 0.95)

p1
