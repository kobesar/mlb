library(tidyverse)
library(rvest)

tab <- read_html("https://www.baseball-reference.com/leagues/majors/2020.shtml") %>% 
  html_nodes('#all_teams_standard_pitching') %>%     # select node with comment
  html_nodes(xpath = 'comment()') %>%   # select comments within node
  html_text() %>%                       # return contents as text
  read_html() %>%                       # parse text as html
  html_node('table') %>%                # select table node
  html_table() 

tab[, 2:36] <- sapply(tab[, 2:36], as.numeric) 

lm(ERA ~ H + FIP, tab)

lm(W ~ ERA + H + FIP, tab)

# Compute predicted runs based on ERA of opposing pitcher, E of opposing team, BA of team, OBP of team, 

teams <- read.csv("https://raw.githubusercontent.com/glamp/mlb-payroll-and-wins/master/team-colors.csv")

batting <- read_html("https://www.baseball-reference.com/leagues/majors/2021.shtml") %>% 
  html_table() %>% 
  .[[1]]

fullba <- cbind(batting[1:30, ], teams) %>% 
  select(tm, BA, OBP)

pitch <- read.csv("pitching2021.csv")

pitch$h_bf <- pitch$H / pitch$BF

pgl2021 <- read.csv("pgl2021.csv")

pgl2021$Rk <- as.numeric(pgl2021$Rk)

pgl2021 <- pgl2021 %>% 
  filter(!is.na(Rk))

pgl2021$win <- str_detect(pgl2021$Rslt, "W")

pgl2021$win <- ifelse(pgl2021$win == TRUE, 1, 0)

fullpitch <- left_join(pgl2021, fullba, by = c("Opp" = "tm")) %>% 
  select(R, ERA, Pit, Str, win, BA, OBP)

fullpitch[, 1:7] <- sapply(fullpitch[, 1:7], as.numeric)

simpl_lm <- lm(R ~ ERA + BA + OBP, fullpitch)

summary(simpl_lm)

ba <- runif(30, min = 0.2, max = 0.35)

obp <- runif(30, min = 0.3, max = 0.36)

yusei <- data.frame(matrix(nrow = 30))
yusei$ERA <- 3.46 
yusei$BA <- ba
yusei$OBP <- obp

yusei <- yusei[, -1]

yusei$runs <- predict(simpl_lm, yusei)

carlos <- data.frame(matrix(nrow = 30))
carlos$ERA <- 1.83 
carlos$BA <- ba
carlos$OBP <- obp

carlos <- carlos[, -1]

carlos$runs <- predict(simpl_lm, carlos)

ggplot() +
  geom_point(data = yusei, aes(x = BA, y = runs), color = "blue") + 
  geom_point(data = carlos, aes(x = BA, y = runs), color = "red")

