library(tidyverse)
library(rvest)
library(V8)

colors <- read.csv("https://raw.githubusercontent.com/glamp/mlb-payroll-and-wins/master/team-colors.csv")

html <- read_html("https://www.spotrac.com/mlb/payroll/")

tab <- html %>% html_nodes("#main") %>% 
  html_table() %>% 
  .[[1]]

colnames(tab) <- c("rank", "tm", "winp",  "ros","pay26", "ir", "ret", "bur", "sus", "tot")

tab <- tab %>% 
  filter(rank != "League Average")

str_remove(tab$tot, "$")

sapply(tab$tot, str_remove, "$")

tab$tot <- as.numeric(gsub("[\\$,]", "", tab$tot))
tab$winp <- as.numeric(tab$winp)

tab$tmabb <- sapply(str_split(sapply(str_split(tab$tm, " "), tail, 1), "\t"), tail, 1)

tm_colors <- colors[, 2:3]

tab[tab$tmabb == "SD" , 11] <- "SDP"

tab[tab$tmabb == "WSH" , 11] <- "WSN"

tab[tab$tmabb == "KC" , 11] <- "KCR"

tab[tab$tmabb == "SF" , 11] <- "SFG"

tab[tab$tmabb == "TB" , 11] <- "TBR"

tab[tab$tmabb == "MIA" , 11] <- "FLA"

tab <- left_join(tab, tm_colors, by = c("tmabb" = "tm"))

plot <- tab %>% 
  ggplot() +
  geom_point(aes(x = tot, y = winp), color = "dodgerblue4") +
  geom_label(data = subset(tab, tmabb == "CLE"), aes(x = tot, y = winp, label = tmabb), vjust = 0, nudge_y = 0.005) +
  geom_label(data = subset(tab, tmabb == "SEA"), aes(x = tot, y = winp, label = tmabb), vjust = 0, nudge_y = 0.005) +
  geom_label(data = subset(tab, tmabb == "NYY"), aes(x = tot, y = winp, label = tmabb), vjust = 0, nudge_y = 0.005) +
  scale_color_manual(values = tab$team_color) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 15),
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#F9F9F9", color = "white")) +
  labs(
    x = "2021 Total Salary Cap",
    y = "Win Percentage",
    caption = "@swingmisstake | sportrac.com")

ggsave("winpcap.png", plot, width = 10, height = 6, units = "in")
