library(tidyverse)

kelenic <- read.csv("~/desktop/kelenic.csv")

features <- c(4:6, 16, 20, 29:32, 46:51, 57:59)

kelenic_scaled <- kelenic[features]

kelenic_scaled$p_throws <- ifelse(kelenic_scaled$p_throws == "R", 1, 0)

kelenic_scaled <- as.data.frame(scale(kelenic_scaled))

kel_dist <- dist(kelenic_scaled, method = 'euclidean')

hclust_avg <- hclust(kel_dist, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 3)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

kelenic$cluster <- cut_avg

kelenic %>% 
  ggplot() +
  geom_point(aes(x = plate_x, y = plate_z, color = as.factor(cluster)))
