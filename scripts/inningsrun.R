library(tidyverse)
library(rvest)



url <- "https://stathead.com/baseball/inning_summary.cgi?year=2021&team_id=SEA&utm_medium=br&utm_source=team-inning-sum&utm_campaign=baseball"

url <- "https://stathead.com/baseball/inning_summary.cgi?year=2021&team_id=%s&utm_medium=br&utm_source=team-inning-sum&utm_campaign=baseball#runs_scored_by_inning"

result <- data.frame(matrix(ncol = 10))

colnames(result) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "team")

for (team in tm_abb$tm_lng) {
  
  if (team == "TBR") {
    nteam <- "TBD"
  } else if (team == "LAA") {
    nteam <- "ANA"
  } else if (team == "MIA") {
    nteam <- "FLA"
  } else if (team == "WSH") {
    nteam <- "WSN"
  } else {
    nteam <- team
  }
  
  tab <- sprintf(url, nteam) %>% read_html() %>%                   # parse html
    html_nodes('#all_runs_scored_by_inning') %>%     # select node with comment
    html_nodes(xpath = 'comment()') %>%   # select comments within node
    html_text() %>%                       # return contents as text
    read_html() %>%                       # parse text as html
    html_node('table') %>%                # select table node
    html_table() 
  
  imp <- unlist(tab[1:9, 11])
  
  result <- rbind(result, c(imp, team))
}

result <- result %>% 
  tail(-1)
result

pivot_wider(result,  values_from = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

inn_scored <- pivot_longer(result, cols = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

colnames(inn_scored) <- c("team", "inning", "runs")

inn_scored$inning <- as.numeric(inn_scored$inning)

inn_scored$runs <- as.numeric(inn_scored$runs)

plot <- inn_scored %>% 
  group_by(inning) %>% 
  summarize(runs = mean(runs)) %>% 
  ggplot() +
  geom_line(aes(x = inning, y = runs), linetype = "dashed", alpha = 0.5) +
  geom_line(data = inn_scored[inn_scored$team == "SEA", ], aes(x = inning, y = runs), color = "#0C2C56") +
  theme_minimal() +
  theme(plot.title = element_text(face = "italic"),
        plot.caption = element_text(hjust = 1, color = "#696969"),
        text = element_text(family = "mono"),
        plot.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9"),
        panel.background = element_rect(fill = "#F9F9F9", color = "#F9F9F9")) +
  labs(x = "Inning", y = "Runs", title = "Total Runs Scored by Inning", caption = "@swingmisstake | stathead.com")

ggsave("runsscored.png", plot, width = 8, units = "in")
