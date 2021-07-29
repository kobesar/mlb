library(tidyverse)
library(rpart)
library(rvest)
library(httr)
library(htmltools)

team_colors <- read.csv("https://raw.githubusercontent.com/glamp/mlb-payroll-and-wins/master/team-colors.csv")

pitching_df <- read.csv("~/Desktop/projects/mlb/pitching.csv")

basic_fit <- rpart(W ~ ., data = pitching_df, method = "class")

predicted <- predict(basic_fit, pitching_df, type = "class")

accuracy <- length(which(data[,'W'] == predicted)) / length(predicted) * 100

get_data <- function(years, batting) {
  
  if (batting) {
    url <- "https://www.baseball-reference.com/leagues/MLB/%s-standard-batting.shtml"
    node_string <- "#div_teams_standard_batting"
  } else {
    url <- "https://www.baseball-reference.com/leagues/MLB/%s-standard-pitching.shtml"
    node_string <- "#div_teams_standard_pitching"
  }
  
  result <- data.frame()
  
  for (year in years) {
    
    html <- read_html(
      sprintf(url, year))
    
    table <- html %>% 
      html_nodes(node_string) %>% 
      html_table() %>% 
      data.frame() %>% 
      filter(row_number() <= n()-3) %>% 
      mutate(year = year) %>% 
      relocate(year) 
    
    result <- rbind(result, table)
  }
  
  result
}

batting_data <- get_data(c(2000:2021), TRUE)

pitching_data <- get_data(c(2000:2021), FALSE)

combined_data <- left_join(batting_data, pitching_data, by = c("year", "Tm"))

batting <- batting_data %>% 
  select(c(year, Tm, BA, OBP))
  
pitching <- pitching_data %>% 
  select(c(year, Tm, W.L., ERA))

combined <- left_join(batting, pitching, by = c("year", "Tm"))

combined <- left_join(combined, team_colors, by = c("Tm" = "tm"))

combined[c(3:6)] <- lapply(combined[c(3:6)], as.numeric)

combined <- combined %>% 
  filter(!is.na(team_name))

combined[is.na(combined)] <- 0

team_cols <- team_colors$team_color

names(team_cols) <- team_colors$tm
 
ggplot(combined) +
  geom_point(aes(x = ERA, y = BA, color = Tm)) +
  scale_color_manual(values = team_cols)

combined_svd <- svd(combined_data[-c(1:2)],)

combined %>% 
  filter(Tm == "SEA") %>% 
  ggplot() +
  geom_point(aes(x = ERA, y = BA))
