library(tidyverse)
library(rvest)

teams <- read.csv("https://raw.githubusercontent.com/glamp/mlb-payroll-and-wins/master/team-colors.csv")

html <- read_html("https://baseballsavant.mlb.com/savant-player/marco-gonzales-594835?stats=gamelogs-r-pitching-mlb&season=2021")
tabs <- html %>% html_table()

gl_21 <- tabs[[33]]
