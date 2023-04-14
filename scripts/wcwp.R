library(baseballr)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(ggtext)

theme_kobe <- theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        plot.title = element_markdown(size = 30, halign = T, vjust = -0.1),
        plot.subtitle = element_markdown(size = 15, hjust = 0.02)
        )

wc_games <- mlb_game_pks(Sys.Date())

sea_tor_2 <- mlb_game_wp(wc_games[2, 1])

sea_tor_2_pbp <- mlb_pbp(wc_games[2, 1])

sea_tor <- sea_tor_2 %>%
  mutate(diff = lead(away_team_win_probability) - away_team_win_probability) %>%
  left_join((
    sea_tor_2_pbp %>%
      mutate(atBatIndex = as.numeric(atBatIndex) - 1) %>%
      group_by(atBatIndex) %>%
      slice(1) %>%
      select(
        atBatIndex,
        matchup.batter.id,
        result.description,
        batting_team
      ) %>%
      mutate(
        prof_pic = sprintf(
          'https://img.mlbstatic.com/mlb-photos/image/upload/w_180,q_100/v1/people/%s/headshot/silo/current',
          matchup.batter.id
        )
      )
  ),
  by = c("at_bat_index" = "atBatIndex")
  )

sea_tor$diff <- c(-2.2, sea_tor$diff) %>% head(-1)

sea_tor <- sea_tor %>%
  mutate(i = 1:n())

sea_tor <- sea_tor %>%
  # mutate(label = paste0(result.description, ' ', ifelse(diff < 0, '', '+'), diff, 'WP')) %>%
  # mutate(stuff = paste0("<img src='", prof_pic, "'> <p>", label, "</p>")) %>%
  mutate(wp = paste0(ifelse(diff < 0, '', '+'), diff, 'WP'),
         label = str_replace_all(result.description , "   ", "\n"))

sea_tor %>%
  ggplot() +
  geom_line(aes(x = at_bat_index, y = away_team_win_probability)) +
  geom_point(
    data = sea_tor[sea_tor$diff >= 10 &
                     sea_tor$batting_team == "Seattle Mariners",],
    aes(x = at_bat_index, y = away_team_win_probability),
    color = "#005C5C",
    size = 3
  ) +
  # geom_point(data = sea_tor[sea_tor$diff <= -10 & sea_tor$batting_team == "Seattle Mariners", ], aes(x = i, y = away_team_win_probability), color = "red") +
  geom_label_repel(
    data = sea_tor[sea_tor$diff >= 10 &
                     sea_tor$batting_team == "Seattle Mariners",],
    aes(x = at_bat_index, y = away_team_win_probability, label = label),
    family = "Roboto Condensed",
    point.padding = 0.5,
    nudge_y = 5,
    nudge_x = -20,
    size = 3,
  ) +
  geom_text(
    data = sea_tor[sea_tor$diff >= 10 &
                     sea_tor$batting_team == "Seattle Mariners",],
    aes(x = at_bat_index, y = away_team_win_probability + 3, label = wp),
    size = 3
  ) +
  geom_point(data = sea_tor[sea_tor$diff <= -10 &
                              sea_tor$batting_team == "Toronto Blue Jays",],
             aes(x = at_bat_index, y = away_team_win_probability),
             color = 'red',
             size = 3) +
  geom_label_repel(
    data = sea_tor[sea_tor$diff <= -10 &
                     sea_tor$batting_team == "Toronto Blue Jays",],
    aes(x = at_bat_index, y = away_team_win_probability, label = label),
    family = "Roboto Condensed",
    point.padding = 0.8,
    nudge_y = -10,
    nudge_x = -20,
    size = 3,
  ) +
  geom_text(
    data = sea_tor[sea_tor$diff <= -10 &
                     sea_tor$batting_team == "Toronto Blue Jays",],
    aes(x = at_bat_index, y = away_team_win_probability + 3, label = wp),
    size = 3,
    nudge_x = -2,
    nudge_y = -3
  ) +
  # geom_image(data = sea_tor[sea_tor$diff >= 10 & sea_tor$batting_team == "Seattle Mariners", ], aes(x = i, y = away_team_win_probability, image = prof_pic)) +
  labs(x = "At Bat #", y = "Win Probability", title = "Win Probability of the Mariners During the Wild Card Game", subtitle = '(Wild Card Game 2 - 10/08/2022)', caption = '@swingmisstake | data: baseballr') +
  theme_kobe

ggsave('~/desktop/wp.png', width = 14, height = 8, units = 'in')


sea_tor_2_pbp <- sea_tor_2_pbp %>%
  mutate(atBatIndex = as.numeric(atBatIndex)) %>%
  left_join(sea_tor_2, by = c("atBatIndex" = "at_bat_index"))

sea_tor_
