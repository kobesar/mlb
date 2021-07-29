library(tidyverse)
library(V8)

html <- read_html("https://baseballsavant.mlb.com/gamefeed?date=6/25/2021&gamePk=633620&chartType=pitch&legendType=pitchName&playerType=pitcher&inning=&count=&pitchHand=&batSide=&descFilter=&ptFilter=&resultFilter=&hf=playerBreakdown#633620")

html %>% 
  html_nodes("#gamefeed_gamefeed") %>% 
  html_nodes("#article-template") %>% 
  html_text()

js <- html %>% 
  html_nodes("script") %>% 
  .[[8]] %>% 
  html_text()

ct <- v8()

read_html(ct$eval(gsub('document.write','', js))) %>% 
  html_text()

ct$eval(gsub('document.write','', js)) %>% 
  print()

html %>% 
  html_nodes("#gamefeed_gamefeed") %>% 
  html_nodes("#games") %>% 
  html_nodes("#game-data") %>% 
  html_text()

html %>% 
  html_nodes("#article-template")

jp <- read_html("https://www.baseball-reference.com/players/gl.fcgi?id=crawfjp01&t=b&year=2021#batting_gamelogs") %>% 
  html_table() %>% 
  .[[5]] %>% 
  head(-1) %>% 
  data.frame()

jp$H <- as.numeric(jp$H)

jp <- jp %>% 
  filter(!is.na(H))

hs <- c()

for (i in 1:length(jp$H)) {
  if (i == 1) {
    if (jp[i, 13] != 0) {
      hs[i] <- 1
    } else {
      hs[i] <- 0
    }
  } else {
    if (jp[i, 13] != 0) {
      hs[i] <- hs[i - 1] + 1
    } else {
      hs[i] <- 0
    }
  }
}

jp$hs <- hs

jp %>% 
  ggplot() +
  geom_area(aes(x = Gtm, y = hs), group = 1)

add_