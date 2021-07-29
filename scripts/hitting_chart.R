library(tidyverse)
library(rvest)
library(lubridate)
library(scales)

jp <- read_html("https://www.baseball-reference.com/players/gl.fcgi?id=crawfjp01&t=b&year=2021") %>% 
  html_table() 

teams <- read.csv("https://raw.githubusercontent.com/glamp/mlb-payroll-and-wins/master/team-colors.csv")

jp_gl <- jp[[5]] %>% 
  data.frame()

jp_gl$Date <- as.Date(jp_gl$Date, "%B %d")
jp_gl$Date <- ymd(jp_gl$Date)

jp_gl %>% 
  filter(Date > "2021-06-14" & Date < "2021-06-24")

jp_imp <- jp_gl %>% 
  filter(Date > "2021-06-14" & Date < "2021-06-24") %>% 
  select(Date, AB, H, BA, Opp, Rslt)

jp_imp %>% 
  ggplot(aes(x = as.factor(Date))) +
  geom_bar(aes(y = as.numeric(H)), stat = "identity", fill = teams$team_color[teams$tm == "SEA"], group = 1) +
  geom_line(aes(y = as.numeric(BA)), group = 1) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "Number of Hits",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.01, name="Batting Average")
  ) + 
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    y = "Hits",
    caption = "https://www.baseball-reference.com/")

jp_plot <- jp_gl %>% 
  filter(!is.na(Date)) %>% 
  ggplot() +
  geom_line(aes(x = Date, y = as.numeric(BA)), color = teams$team_color[teams$tm == "SEA"], group = 1) +
  geom_hline(aes(yintercept = 0.238), linetype = "dashed", color = "red") +
  annotate("text", x = ymd("2021-06-15"), y = 0.238, label = "League BA", vjust = -0.5) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    y = "BA",
    caption = "https://www.baseball-reference.com/")

ggsave("jp_ba.png", jp_plot, width = 8, height = 5, units = "in")

hitting_chart <- function(url) {
  player <- read_html(url) %>% 
    html_table() 
  
  gl <- player[[5]] %>% 
    data.frame()

  gl$Date <- ymd(jp_gl$Date)
  
  gl %>% 
    filter(Date > "2021-06-14" & Date < "2021-06-24")
  
  plot <- gl %>% 
    filter(!is.na(Date)) %>% 
    ggplot() +
    geom_line(aes(x = Date, y = as.numeric(BA)), color = teams$team_color[teams$tm == "SEA"], group = 1) +
    geom_hline(aes(yintercept = 0.238), linetype = "dashed", color = "red") +
    annotate("text", x = ymd("2021-06-15"), y = 0.238, label = "League BA", vjust = -0.5) +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b") +
    theme_minimal() +
    theme(
      legend.position = "none",
      text = element_text(family = "mono"),
      plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
    labs(
      y = "BA",
      caption = "https://www.baseball-reference.com/")
  
  ggsave("ba.png", jp_plot, width = 8, height = 5, units = "in")
}