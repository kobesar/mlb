library(tidyverse)
library(rvest)
library(kableExtra)

bp <- c("Kendall Graveman" = "https://www.baseball-reference.com/players/g/graveke01.shtml", "Drew Steckenrider" = "https://www.baseball-reference.com/players/s/steckdr01.shtml",  "Paul Sewald"= "https://www.baseball-reference.com/players/s/sewalpa01.shtml",  "J.T. Chargois" = "https://www.baseball-reference.com/players/c/chargjt01.shtml") 

dats <- list()

for (i in 1:length(bp)) {
  pitcher <- names(bp)[i]
  url <- bp[i][[1]]
  
  tab <- read_html(url) %>% 
    html_nodes("#pitching_standard") %>% 
    html_table() %>% 
    .[[1]] %>% 
    data.frame()
  
  dats[[i]] <- tab
}

grave <- dats[[1]] %>% 
  slice(15:16) %>% 
  mutate(player = "Kendall Graveman")

drew <- dats[[2]] %>% 
  slice(12:13) %>% 
  mutate(player = "Drew Steckenrider")

paul <- dats[[3]] %>% 
  slice(14:15) %>% 
  mutate(player = "Paul Sewald")

jt <- dats[[4]] %>% 
  slice(12:13) %>% 
  mutate(player = "J.T. Chargois")

grave_era <- grave %>%
  select(ERA, player)

drew_era <- drew %>% 
  select(ERA, player)

paul_era <- paul %>% 
  select(ERA, player)

jt_era <- jt %>% 
  select(ERA, player)

pitchers <- rbind(grave_era, drew_era, paul_era, jt_era)

pitchers$which <- c("2021 ERA", "Career ERA")

pitchers_clean <- pivot_wider(pitchers, id_cols = player, names_from = which, values_from = ERA)

pitchers_clean$Difference <- pitchers_clean$`Career ERA` - pitchers_clean$`2021 ERA`

pitchers_clean <- pitchers_clean %>% 
  rename("Pitcher" = "player")

kable(pitchers_clean) %>% 
  kable_styling() %>% 
  column_spec(1, underline = TRUE) %>% 
  row_spec(1:4, monospace = TRUE) %>%
  column_spec(4, color = "white",
              background = spec_color(pitchers_clean$`2021 ERA`[1:4], end = 0.7, direction = -1),
              popover = paste("am:", pitchers_clean$`2021 ERA`[1:4])) %>% 
  footnote(general = "data taken from baseball-reference.com")

all <- rbind(grave, drew, paul, jt)

all <- all %>% 
  filter(Year == "2021")

all$so_bf <- round(all$BF / all$SO, 2)

all$bb_bf <- round(all$BF / all$BB, 2)

all_select <- all %>% 
  select(player, so_bf, bb_bf)

colnames(all_select) <- c("Pitcher", "SO Occurence", "BB Occurence")

kable(all_select) %>% 
  kable_styling() %>% 
  column_spec(1, underline = TRUE) %>% 
  row_spec(1:4, monospace = TRUE) %>%
  column_spec(2, color = "white",
              background = spec_color(all_select[1:4, 2], end = 0.6, direction = -1),
              popover = paste("am:", all_select[1:4, 2])) %>% 
  column_spec(3, color = "white",
              background = spec_color(all_select[1:4, 3], end = 0.6, direction = 1),
              popover = paste("am:", all_select[1:4, 3])) %>% 
  footnote(general = "data taken from baseball-reference.com")
