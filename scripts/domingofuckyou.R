library(baseballr)
library(ggrepel)
library(english)
library(patchwork)

data_today <- read.csv("~/desktop/german.csv")

data <- scrape_statcast_savant_pitcher("2022-03-31", "2023-04-15", 593334)

ordinal_text <- function(x) {
  suffix <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
  ifelse(x %% 100 %in% c(11:13), paste0(x, "th"), paste0(x, suffix[x %% 10 + 1]))
}

# Exploratory for last year's trend
data %>% 
  group_by(inning, pitch_type) %>% 
  summarize(rpm = mean(release_spin_rate, na.rm = T), n = n()) %>% 
  ggplot() +
  geom_line(aes(x = inning, y = rpm, color = pitch_type))

data_today %>% 
  group_by(inning, pitch_type) %>% 
  summarize(rpm_today = mean(rpm)) %>% 
  left_join(data %>% 
              group_by(inning, pitch_type) %>% 
              summarize(rpm_2022 = mean(release_spin_rate, na.rm = T)), by = c("inning", "pitch_type")) %>% 
  mutate(diff = rpm_today - rpm_2022) %>% 
  ggplot() +
  geom_line(aes(x = inning, y = diff, color = pitch_type))

data_today %>% 
  arrange(X) %>% 
  mutate(rpm = cummean(rpm)) %>% 
  ggplot() +
  geom_line(aes(x = X, y = rpm))

data %>% 
  group_by(game_pk) %>% 
  filter(inning < 8) %>% 
  arrange(at_bat_number) %>% 
  mutate(pitch_num = 1:n()) %>%
  group_by(game_pk, pitch_num) %>% 
  mutate(rpm = cummean(release_spin_rate)) %>% 
  # select(pitch_num, rpm) %>% 
  ggplot() +
  geom_line(aes(x = pitch_num, y = rpm, color = as.factor(game_pk)))

# Too much variation, we'll go with average the rpm for each pitch over the innings, first get the top 2 pitches from him from last year

top_pitches <- data %>% 
  group_by(pitch_type) %>% 
  summarize(n = n()) %>% 
  arrange(-n) %>% 
  head(2) %>% 
  pull(pitch_type)

data_today %>% 
  filter(pitch_type %in% top_pitches) %>% 
  group_by(inning, pitch_type) %>% 
  summarize(rpm_today = mean(rpm)) %>% 
  left_join(data %>% 
              filter(pitch_type %in% top_pitches) %>% 
              group_by(inning, pitch_type) %>% 
              summarize(rpm_2022 = mean(release_spin_rate, na.rm = T)), by = c("inning", "pitch_type")) %>% 
  pivot_longer(3:4, names_to = "when", values_to = "rpm") %>% 
  # mutate(diff = rpm_today - rpm_2022) %>% 
  ggplot() +
  geom_line(aes(x = inning, y = rpm, color = pitch_type)) +
  facet_wrap(~ when)

p1 <- data_today %>% 
  filter(pitch_type %in% top_pitches) %>% 
  group_by(pitch_type) %>% 
  arrange(X) %>% 
  mutate(rpm = cummean(rpm)) %>% 
  ungroup() %>% 
  mutate(pitch_num = X + 1, inning_change = lead(inning, default = first(inning)) != inning, inning_form = ordinal_text(inning)) %>%
  ggplot() +
  geom_segment(aes(x = pitch_num, xend = pitch_num, y = 2550, yend = ifelse(pitch_num == 23, 2754.625, rpm), alpha = ifelse(pitch_num == 23, TRUE, inning_change)), linetype = 2, color = "#c4ced3") +
  geom_line(aes(x = pitch_num, y = rpm, color = pitch_type)) +
  geom_text(aes(x = pitch_num, y = 2540, label = ifelse(inning == 1, paste0("End of ", inning_form, " Inning"), inning_form), alpha = inning_change), family = "Source Sans Pro", nudge_y = 5, color = "#60758c", size = 4) +
  geom_text(data = (data_today %>% filter(pitch_type %in% top_pitches) %>% 
                      group_by(pitch_type) %>% 
                      mutate(n = 1:n(), pitch_num = X + 1) %>% 
                      filter(n == 1) %>% 
                      mutate(text = ifelse(pitch_type == "FF", "Four-Seam Fastball", "Cutter"))), aes(x = ifelse(pitch_type == "CU", pitch_num + 0.5, pitch_num), y = rpm, label = text, color = pitch_type), family = "Source Sans Pro", hjust = -0.1, vjust = -0.1) +
  # geom_point(aes(x = pitch_num, y = ifelse(pitch_num == 23, 2754.625, rpm), alpha = ifelse(pitch_num == 23, TRUE, inning_change)), color = "#60758c") +
  geom_point(aes(x = pitch_num, y = rpm, color = pitch_type, shape = pitch_type), color = "#0C2340", size = 3, fill = "white") +
  geom_text(data = data.frame(x = 35, y = 2545, label = "Umps check Germán's hand for sticky stuff."), aes(x = x, y = y, label = str_wrap(label, width = 20)), family = "Source Sans Pro", nudge_y = 3, nudge_x = -5, color = "#132448", size = 2.5) +
  geom_text(data = data.frame(x = 36, y = 2545,label = "Rocco Baldelli gest thrown out."), aes(x = x, y = y, label = str_wrap(label, width = 20)), family = "Source Sans Pro", nudge_y = 3, nudge_x = 4, color = "#132448", size = 2.5) +
  scale_y_continuous(labels = function(x) paste0(x, " RPMs")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  scale_color_manual(values = c("CU" = "#132448", "FF" = "#536982")) +
  scale_shape_manual(values = c("CU" = 21, "FF" = 22)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans Pro"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    plot.subtitle = element_text(size = 12, margin = margin(b = 30)),
    plot.title = element_text(size = 20)
  ) +
  labs(x = "", y = "", title = "Domingo Germán's RPM over his start vs. Twins", subtitle = "MIN vs. NYY, 4/15/2023")

data_diff <- data_today %>% 
  group_by(inning, pitch_type) %>% 
  summarize(rpm_today = mean(rpm)) %>% 
  left_join(data %>% 
              group_by(inning, pitch_type) %>% 
              summarize(rpm_2022 = mean(release_spin_rate, na.rm = T)), by = c("inning", "pitch_type")) %>% 
  # mutate(diff = rpm_today - rpm_2022) %>% 
  filter(pitch_type %in% top_pitches & inning == 3 | inning == 4) %>% 
  group_by(pitch_type) %>% 
  arrange(inning) %>% 
  mutate(diff_today = rpm_today - lag(rpm_today), diff_2022 = rpm_2022 - lag(rpm_2022)) %>% 
  select(pitch_type, diff_today, diff_2022) %>% 
  filter(!is.na(diff_today)) %>% 
  pivot_longer(2:3, names_to = "when", values_to = "rpm_diff") %>% 
  mutate(when = ifelse(when == "diff_today", "4/15/2023 vs. MIN", "2022 Avg."))

p2 <- data_diff %>% 
  mutate(pitch_type = ifelse(pitch_type == "FF", "Four-Seam Fastball", "Cutter")) %>% 
  ggplot() +
  geom_bar(aes(x = when, y = rpm_diff, fill = pitch_type, alpha = when == "4/15/2023 vs. MIN"), stat = "identity", width = 0.5) +
  geom_text(aes(x = when, y = rpm_diff, label = paste0(round(rpm_diff), " RPMs")), vjust = ifelse(data_diff$rpm_diff < 0, 1.4, -0.5), family = "Source Sans Pro", size = 3) +
  geom_label(aes(x = when, y = ifelse(rpm_diff < 0, 4, -4), label = when), family = "Source Sans Pro", size = 3) +
  facet_wrap(~ pitch_type, ncol = 2) +
  scale_fill_manual(values = c("Cutter" = "#132448", "Four-Seam Fastball" = "#536982")) +
  scale_y_continuous(labels = function(x) paste0(x, " RPMs"), position = "right") +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.5)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(family = "Source Sans Pro"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.y.right = element_text(face = "bold", size = 11),
    axis.text.x = element_blank(),
    strip.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.7, size = 12, margin = margin(t = 50))
    # axis.title.x = element_text()
  ) +
  labs(y = "", title = "RPM Change From the 3rd to 4th Inning", x = "")

final <- p1 + p2 + plot_layout(widths = c(2, 1))

ggsave(final, "../viz/domingo.png")