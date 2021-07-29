library(tidyverse)
library(rvest)
library(lubridate)
library(rsvg)
library(grid)
library(gtable)

tab <- read_html("https://www.baseball-reference.com/teams/SEA/2021-schedule-scores.shtml#team_schedule") %>% 
  html_nodes("#all_team_schedule") %>% 
  html_table() %>% 
  .[[1]]

diff <- tab[, c(1, 2, 8, 9)]

diff$R <- as.numeric(diff$R)

diff$RA <- as.numeric(diff$RA)

diff <- diff %>% 
  filter(!is.na(R))

diff <- diff %>% 
  mutate(rsum = cumsum(R))
  
diff <- diff %>% 
  mutate(rasum = cumsum(RA))

diff <- diff %>% 
  rename("gm" = "Gm#")

diff$gm <- as.numeric(diff$gm)

diff$diff <- diff$R - diff$RA

diff$dt <- str_split(diff$Date, "\\(") %>% lapply('[[', 1)

diff$dt <- format(diff$dt, format = "%A, %B %d") %>% 
  as.Date(format = "%A, %B %d")

cumrun <- diff %>% 
  ggplot(aes(x = dt)) +
  geom_line(aes(y = rsum), color = "dodgerblue3", group = 1) + 
  geom_line(aes(y = rasum), color = "darkred", group = 1) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(title = "", x = "", y = "Runs", caption = "@swingmisstake | www.baseball-reference.com")

ggsave("../vis/cumrun.png", cumrun)

diff %>% 
  ggplot() +
  geom_bar(aes(x = dt, y = diff), stat = "identity", fill = ifelse(diff$diff > 0, "dodgerblue3", "darkred")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(title = "2021 Mariners Run Differential", x = "", y = "Runs", caption = "@swingmisstake | www.baseball-reference.com")

mcum <- diff %>% 
  ggplot() +
  geom_line(aes(x = dt, y = abs(rsum - rasum)), color = "#0C2C56") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic", size = 20),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(title = "2021 Mariners Cumulative Run Differential", x = "", y = "Runs", caption = "@swingmisstake | www.baseball-reference.com")
  
mcums <- ggplotGrob(mcum)

ms_logo <- png::readPNG("../logos/logos_short/SEA.png") %>% 
  rasterGrob(interpolate = TRUE)

new.title <- gtable(widths = grobWidth(mcums),
                    heights = grobHeight(mcums)) %>%
  gtable_add_grob(grobs = ms_logo, t = 1, l = 1) %>%
  gtable_add_cols(widths = unit(1, "null")) %>%
  gtable_add_grob(textGrob(label = "2021 Mariners Cumulative Run Differential",
                           x = unit(0, "npc"), just = "left"),
                  t = 1, l = 2) %>%
  # optional: adda fixed amt of space between image & text
  gtable_add_col_space(width = unit(5, "pt")) 

mcums$grobs[[which(mcums$layout$name == "title")]] <- new.title

grid.draw(mcums)

ggsave("mcums.png", mcums)
