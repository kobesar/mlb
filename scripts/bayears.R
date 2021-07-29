library(tidyverse)
library(rvest)
library(gganimate)

tab <- read_html("https://www.baseball-reference.com/leagues/majors/2021-standard-batting.shtml") %>% 
  html_nodes('#all_players_standard_batting') %>%     # select node with comment
  html_nodes(xpath = 'comment()') %>%   # select comments within node
  html_text() %>%                       # return contents as text
  read_html() %>%                       # parse text as html
  html_node('table') %>%                # select table node
  html_table() %>% 
  data.frame()

tab$BA <- as.numeric(tab$BA)

tab$PA <- as.numeric(tab$PA)

tab <- tab %>% 
  filter(!is.na(BA) & PA > 50)

tabs_list <- list()

for (year in 1876:2021) {
  tab <- read_html(sprintf("https://www.baseball-reference.com/leagues/majors/%s-standard-batting.shtml", year)) %>% 
    html_nodes('#all_players_standard_batting') %>%     # select node with comment
    html_nodes(xpath = 'comment()') %>%   # select comments within node
    html_text() %>%                       # return contents as text
    read_html() %>%                       # parse text as html
    html_node('table') %>%                # select table node
    html_table() %>% 
    data.frame()
  
  tab$BA <- as.numeric(tab$BA)
  
  tab$PA <- as.numeric(tab$PA)
  
  tab <- tab %>% 
    filter(!is.na(BA) & PA > 50)
  
  year <- str_c(year, "")
  
  tabs_list[[year]] <- tab
}

all_ba <- data.frame(matrix(ncol = 2))

colnames(all_ba) <- c("BA", "yr")

for (year in names(tabs_list)) {
  rows <- tabs_list[[year]] %>% 
    select(BA) %>% 
    mutate(yr = year)
  
  all_ba <- rbind(all_ba, rows)
}

all_ba$yr <- as.integer(all_ba$yr)

all_ba <- all_ba %>% 
  tail(-1)

all_ba %>% 
  ggplot() + 
  geom_histogram(aes(BA)) + 
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        legend.position =  "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  transition_time(yr) +
  labs(title = "BA Distribution for {frame_time}", caption = "@swingmisstake | www.baseball-reference.com")  +
  shadow_wake(wake_length = 0.1, alpha = FALSE)

all_ba %>% 
  group_by(yr) %>% 
  summarize(ba = mean(BA)) %>% 
  ggplot() +
  geom_line(aes(x = yr, y = ba)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        legend.position =  "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  transition_reveal(yr) +
  labs(x = "Year", y = "BA", title = "BA Over Time", caption = "@swingmisstake | www.baseball-reference.com") 

