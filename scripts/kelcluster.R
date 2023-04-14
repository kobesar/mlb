library(tidyverse)

kelenic <- read.csv("~/desktop/uw/spring3/cse412/a3/kelenic.csv")

kelenic <- kelenic %>% 
  mutate(p_throws = ifelse(p_throws == "R", 1, 0))

features <- c("release_speed", "release_pos_x", "release_pos_z", "release_extension", "spin_axis", "effective_speed", "release_spin_rate", "vx0", "vy0", "vz0", "ax", "ay", "az", "pfx_z", "pfx_x", "p_throws")

kelenic_clean <- kelenic[features]

kelenic_clean[is.na(kelenic_clean)] <- 0

kel_svd <- svd(kelenic_clean)

kelenic_clean$x <- kel_svd$u[, 1]
kelenic_clean$y <- kel_svd$u[, 2]

kelenic_clean$xba <- kelenic$estimated_ba_using_speedangle

kelenic_clean$pitch <- kelenic$pitch_name

kelenic_clean$barrel <- kelenic$barrel

kelenic_clean %>% 
  ggplot() +
  geom_point(aes(x = x, y = y)) +
  facet_wrap (~ as.factor(barrel))



fit <- kmeans(kelenic_clean, 8)
aggregate(kelenic_clean,by=list(fit$cluster),FUN=mean)
kel_cluster <- data.frame(kelenic, fit$cluster)

kel_cluster %>% 
  ggplot() +
  geom_point(aes(x = hc_x, y = hc_y, color = pitch_name)) +
  facet_wrap(~ as.factor(fit.cluster))

kel_cluster %>% 
  group_by(fit.cluster, pitch_type) %>% 
  summarize(xba = mean(estimated_ba_using_speedangle, na.rm = T), n = n(), rl = mean(p_throws, na.rm = T)) %>% View()
