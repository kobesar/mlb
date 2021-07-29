library(rvest)
library(tidyverse)

result <- data.frame()

for (year in 2011:2021) {
  url <- sprintf("https://www.baseball-reference.com/players/gl.fcgi?id=seageky01&t=b&year=%s", year)
  
  table <- data.frame()
  
  table <- read_html(url) %>%
    html_table()
  
  table <- table[[5]] %>% 
    head(-1)
  
  table <- table[,colSums(is.na(table))<nrow(table)]
  
  table <- table[-6] %>% 
    filter(Tm == "SEA")
  
  print(year)
  
  table <- table %>% 
    filter(!is.na(PA)) %>% 
    mutate(cumhits = cumsum(H), year = year)
  
  prio <- table %>% 
    select(Gcar, Gtm, BA, RBI, OPS, OBP, cumhits, year)
  
  result <- rbind(result, prio)
}

result$Gtm <- sub("\\(.*", "", result$Gtm)
result$Gtm <- as.numeric(str_trim(result$Gtm))

result %>% 
  group_by(Gtm) %>% 
  summarize(ba = mean(as.numeric(BA)), gm = Gtm) %>% 
  ggplot() +
  geom_point(aes(x = gm, y = ba), size = 0.3)
  
