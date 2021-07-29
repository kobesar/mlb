library(tidyverse)
library(rvest)
library(rjson)
library(lubridate)
library(directlabels)
library(ggrepel)

txt <- read.delim("../data/yusei") %>% 
  data.frame()

cols <- trimws(lapply(str_split(txt[1:28 , 1], ":"), `[[`, 1))

yusei <- data.frame(matrix(ncol = 28))

colnames(yusei) <- cols

txt <- sapply(txt, str_replace, ",", "")

txt <- txt[gsub("[^A-Za-z()-]", "", txt) != ""] %>% 
  data.frame()

for (i in 1:416) {
  if (i == 1) {
    mult <- 1
    
    first <- 1
    
    last <- 28
  } else {
    mult <- 28
    
    first <- i * mult + 1
    
    last <- i * mult + 28
  }

  row <- txt[first:last, 1]
  
  pay <- lapply(str_split(row, ":"), `[[`, 2)
  
  print(pay)
  
  yusei <- rbind(yusei, pay)
}

yusei <- yusei[-1 ,]

yusei$gd <- as.Date(yusei$gd)

yusei %>% 
  filter(event == "home_run") %>% 
  group_by(name) %>% 
  count() %>% 
  top_n(n)

yusei %>% 
  filter(event == "home_run") %>% 
  group_by(pn) %>% 
  count()

yusei %>% 
  group_by(pn) %>% 
  summarize(ev = mean(ev, na.rm = T))

yusei %>% 
  group_by(bat_team_id) %>% 
  summarize(hr = sum(is_hr, na.rm = T))

yusei %>% 
  group_by(event) %>% 
  summarize(v = mean(as.numeric(v), na.rm = T))

date_pitch <- yusei %>% 
  filter(event == "strikeout") %>% 
  group_by(gd, pn) %>% 
  summarize(v = mean(as.numeric(v)))

plot <- date_pitch %>% 
  mutate(label = if_else(gd == "2021-07-01", as.character(pn), "")) %>%
  ggplot(aes(x = ymd(gd), y = v, color = pn), group = pn) +
  geom_point() +
  geom_line()  +
  scale_x_date(date_labels = "%b") +
  geom_label_repel(aes(label = label, family = "mono"),
                   label.size = NA,
                   fill = "#F9F9F9", 
                   nudge_x = 16,
                   na.rm = TRUE,
                   segment.color = 'transparent',
                   box.padding = 0) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        legend.position =  "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(x = "month", y = "velocity (mph)" ,title = "Koochie's Strikeout Pitch Speed 2021", caption = "@swingmisstake | www.baseballsavant.mlb.com")

ggsave("koochie.png", plot, width = 10, height = 6, units = "in")

plot <- yusei %>%
  group_by(gd) %>% 
  summarize(ev = mean(as.numeric(ev), na.rm = T)) %>% 
  ggplot(aes(x = gd, y = ev)) +
  geom_point() + 
  geom_line() +
  scale_x_date(date_labels = "%b") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        legend.position =  "none",
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(x = "month", y = "ev (mph)" ,title = "Koochie's EV", caption = "@swingmisstake | www.baseballsavant.mlb.com")

ggsave("koochieev.png", plot, width = 10, height = 6, units = "in")

write.csv(yusei, "yusei.csv")
