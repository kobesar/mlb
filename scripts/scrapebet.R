library(tidyverse)
library(rvest)
library(readxl)

tm_abb <- read.csv("https://gist.githubusercontent.com/kobesar/625520d26d9a895cb754e93686a12628/raw/be4ee2c1044e2185be11a863c901797fa9bc6582/mlbteams.csv")

html <- read_html("https://www.espn.com/mlb/lines")

lines <- html %>% 
  html_nodes(xpath = '//*[@class="tablehead"]') %>% 
  html_table()

tab <- lines[[1]] %>% 
  filter(str_detect(X1, "MGM") | str_detect(X1, "Starting Pitchers"))

lines_tab <- data.frame(matrix(ncol = 13))

colnames(lines_tab) <- c("away", "home", "away_line", "home_line", "ou", "away_pitcher", "home_pitcher", "away_arm", "home_arm", "away_era", "home_era", "away_record", "home_record")

for (i in 1:(length(tab$X2)/2)) {
  if (str_detect(tab[i * 2 - 1, 2], "N/A")) {
    tms <- c("TBA", "TBA")
    lines <- c("TBA", "TBA")
    ou <- "TBA"
  } else {
    tms <- trimws(str_split(tab[i * 2 - 1, 2], "[^a-zA-Z]")[[1]])
    lines <- trimws(str_split(tab[i * 2 - 1, 2], "[a-zA-Z]")[[1]])
    ou <- trimws(str_split(tab[i * 2 - 1, 3], " ")[[1]][1])
  }
  
  pitchers <- str_split(str_split(tab[i * 2, 2], "vs.")[[1]], " ")
  
  if (length(pitchers) < 2) {
    city <- trimws(str_remove(pitchers[[1]][1], "[[:punct:]]"))
    team <- tm_abb[str_detect(tm_abb$team, city), 2]
    
    if (team == tms[1]) {
      away_pitcher <- pitchers[[1]][pitchers[[1]] != ""]
      away_record <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", away_pitcher), ",")[[1]][1]
      away_era <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", away_pitcher), ",")[[1]][2]
      away_arm <- str_match_all(away_pitcher, "(?<=\\().+?(?=\\))")[[1]][,1]
      away_pitcher <- gsub(".*: (.+) \\(.*", "\\1", away_pitcher)
      
      home_era <- "TBA"
      home_arm <- "ERA"
      home_pitcher <- "TBA"
    } else {
      home_pitcher <- pitchers[[1]][pitchers[[1]] != ""]
      home_record <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", home_pitcher), ",")[[1]][1]
      home_era <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", home_pitcher), ",")[[1]][2]
      home_arm <- str_match_all(home_pitcher, "(?<=\\().+?(?=\\))")[[1]][,1]
      home_pitcher <- gsub(".*: (.+) \\(.*", "\\1", home_pitcher)
      
      away_era <- "TBA"
      away_arm <- "TBA"
      away_pitcher <- "TBA"
    }
  } else {
    away_pitcher <- paste(unlist(pitchers[[1]]), collapse = ' ')
    home_pitcher <- paste(unlist(pitchers[[2]]), collapse = ' ')
    
    away_record <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", away_pitcher), ",")[[1]][1]
    home_record <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", home_pitcher), ",")[[1]][1]
    
    away_era <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", away_pitcher), ",")[[1]][2]
    home_era <- str_split(gsub(".*\\) (.+) ERA.*", "\\1", home_pitcher), ",")[[1]][2]
    
    away_arm <- str_match_all(away_pitcher, "(?<=\\().+?(?=\\))")[[1]][,1]
    home_arm <- str_match_all(home_pitcher, "(?<=\\().+?(?=\\))")[[1]][,1]
    
    away_pitcher <- gsub(".*: (.+) \\(.*", "\\1", away_pitcher)
    home_pitcher <- gsub(".*: (.+) \\(.*", "\\1", home_pitcher)
  }
  
  tms <- tms[tms != ""]
  lines <- lines[lines != ""]
  
  lines <- trimws(str_remove(lines, "[[:punct:]]"))
  
  # if (lines == "") {
  #   lines[1] <- "NA"
  #   lines[2] <- "NA"
  # }
  
  row <- c(tms, lines, ou, away_pitcher, home_pitcher, away_arm, home_arm, away_era, home_era, away_record, home_record)
  
  lines_tab <- rbind(lines_tab, row)
}

lines_tab <- lines_tab %>% 
  tail(-1)

lines_tab <- left_join(lines_tab, tm_abb, by = c("away" = "tm_short")) %>% 
  rename("away_team" = "team")

lines_tab <- left_join(lines_tab, tm_abb, by = c("home" = "tm_short")) %>% 
  rename("home_team" = "team")

lines_tab

for (year in 2010:2020) {
  url <- sprintf("https://sports-statistics.com/database/mlb-data/mlb%20odds%20%s.xlsx", year)
  
  read_excel(url)
}

mlb2010 <- read_excel("~/desktop/projects/mlb/data/2010.xlsx")

mlb2010 %>% 
  group_by(Close) %>% 
  count()

nfl <- read_html("https://www.espn.com/nfl/scoreboard/_/year/2021/seasontype/2/week/1#")

