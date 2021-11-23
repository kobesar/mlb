library(tidyverse)
library(rvest)
library(kableExtra)

sp <- c("sheffju01", "kikucyu01", "gonzama02", "flexech01", "gilbelo01", "dunnju01", "duggero01")

sp_names <- c("Justus Sheffield", "Yusei Kikuchi", "Marco Gonzales", "Chris Flexen", "Logan Gilbert", "Justin Dunn", "Robert Dugger")

names(sp) <- sp_names

res <- data.frame(matrix(ncol = 4))

colnames(res) <- c("pitcher", "era", "so", "starts")

for (i in 1:length(sp)) {
  
  pitcher <- names(sp)[i]
  
  # url <- sprintf("https://www.baseball-reference.com/players/gl.fcgi?id=%s&t=p&year=2021", sp[i][[1]])
  # 
  # tab <- read_html(url) %>% 
  #   html_table() %>% 
  #   .[[1]] %>% 
  #   data.frame()
  # 
  # colnames(tab)[6] <- "Loc" 
  # 
  # tab <- tab %>%
  #   filter(Tm == "SEA" & Loc == "")
  # 
  # er <-  sum(as.numeric(tab$ER))
  # 
  # ip <- sum(as.numeric(tab$IP)) 
  # 
  # era <- (9 * er) / ip
  # 
  # so <- sum(as.numeric(tab$SO))
  # 
  # row <- c(pitcher, era, so, nrow(tab))
  
  url <- sprintf("https://www.baseball-reference.com/players/split.fcgi?id=%s&year=2021&t=p", sp[i][[1]])
  
  
  
  res <- rbind(res, row)
}

res %>% tail(-1)

read_html("https://baseballsavant.mlb.com/savant-player/chris-flexen-623167?stats=splits-r-pitching-mlb&season=2021") %>% 
  html_nodes("#mlb_splits") %>% 
  html_nodes("#type-splits-mlb") %>% 
  html_table()
   

players <- read.csv("https://www.smartfantasybaseball.com/PLAYERIDMAPCSV")

res <- data.frame()

for (i in 1:length(sp)) {
  
  pitcher <- names(sp)[i] 
  
  id <- players %>% 
    filter(BREFID == sp[i][[1]]) %>% 
    pull(MLBID)
  
  split <- str_to_lower(str_split(pitcher, " ")[[1]])
  
  full <- str_c(split[1], "-", split[2], "-", id)
  
  url <- sprintf("https://baseballsavant.mlb.com/savant-player/%s?stats=splits-r-pitching-mlb&season=2021", full)
  
  tab <- read_html(url)  %>% 
    html_nodes("#mlb_splits") %>% 
    html_nodes("#type-splits-mlb") %>% 
    html_table()
  
  row <- tab[[1]][1, ]
  
  row$Name <- pitcher
  
  res <- rbind(res, row)
}

res_no <- res %>% 
  head(-1)

res_fin <- res_no %>% 
  select(Name, ERA, ER, IP, SO, GS) %>% 
  data.frame()

res_ord <- res_fin[order(res_fin$ERA) ,]

row.names(res_ord) <- NULL

kable(res_ord) %>% 
  kable_styling() %>% 
  column_spec(1, underline = TRUE) %>% 
  row_spec(1:6, monospace = TRUE) %>%
  column_spec(2, color = "white",
              background = spec_color(res_ord$ERA[1:6], end = 0.7, direction = -1),
              popover = paste("am:", res_ord$ERA[1:8])) %>% 
  footnote(general = "data taken from baseballsavant.mlb.com")

res_away <- data.frame()

for (i in 1:length(sp)) {
  
  pitcher <- names(sp)[i] 
  
  id <- players %>% 
    filter(BREFID == sp[i][[1]]) %>% 
    pull(MLBID)
  
  split <- str_to_lower(str_split(pitcher, " ")[[1]])
  
  full <- str_c(split[1], "-", split[2], "-", id)
  
  url <- sprintf("https://baseballsavant.mlb.com/savant-player/%s?stats=splits-r-pitching-mlb&season=2021", full)
  
  tab <- read_html(url)  %>% 
    html_nodes("#mlb_splits") %>% 
    html_nodes("#type-splits-mlb") %>% 
    html_table()
  
  row <- tab[[1]][2, ]
  
  row$Name <- pitcher
  
  res_away <- rbind(res_away, row)
}

res_a <- res_away %>%
  head(-1) %>% 
  select(Name, ERA, ER, IP, SO, GS) %>% 
  data.frame()

res_a <- res_a[order(res_a$ERA) ,]

row.names(res_a) <- NULL

kable(res_a) %>% 
  kable_styling() %>% 
  column_spec(1, underline = TRUE) %>% 
  row_spec(1:6, monospace = TRUE) %>%
  column_spec(2, 
              color = "white",
              background = spec_color(res_ord$ERA[1:6], end = 0.7, direction = -1),
              popover = paste("am:", res_ord$ERA[1:8])) %>% 
  footnote(general = "data taken from baseballsavant.mlb.com")


  