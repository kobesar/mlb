library(baseballr)
library(tidyverse)
library(httr)
library(jsonlite)

mariners <- GET('https://lookup-service-prod.mlb.com/json/named.roster_40.bam?team_id=136') %>% 
  content(as = 'text') %>% 
  fromJSON() %>% 
  .[[1]] %>% 
  .[[2]] %>% 
  .[[3]]

result <- data.frame()

for (i in 1:nrow(mariners)) {
  try({
    tab <- data.frame(scrape_statcast_savant_batter('2022-03-31', Sys.Date(), as.numeric(mariners$player_id[i])))
    if (nrow(tab) > 0) {
      result <- rbind(result, tab)
    }
    })
}

xba_season <- result %>% 
  filter(!is.na(estimated_ba_using_speedangle)) %>% 
  group_by(game_date) %>% 
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T)) %>% 
  mutate(n = 1:n(), ma = cumsum(xba) / n)


