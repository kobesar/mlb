library(tidyverse)

data <- read.csv("~/downloads/pitchers.csv")

lm1 <- lm(p_era ~ n_fastball_formatted + n_breaking_formatted + n_offspeed_formatted, data)

lm2 <- lm(xba ~ n_fastball_formatted + n_breaking_formatted + n_offspeed_formatted, data)

lm3 <- lm(p_era ~ xba, data)

lm1 %>% 
  summary()

lm2 %>% 
  summary()

lm3 %>% 
  summary()

data %>% 
  ggplot(aes(x = xba, y = p_era, color = balance)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  

