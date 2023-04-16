library(tidyverse)
library(tidymodels)

data <- read.csv("../data/pitchcluster22.csv")

data[is.na(data)] <- 0

data_numeric <- as_tibble(data[,c(8:57)])

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(data_numeric, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, data)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = p_era, y = xbadiff)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

mod <- kmeans(data_numeric, 5)

mod$centers %>% View()

data_full <- cbind(data, cluster = mod$cluster)

# Retry with scaling

data_numeric <- data_numeric %>% 
  mutate_all(~scale(.))

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(data_numeric, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, data)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = p_era, y = xbadiff)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

mod <- kmeans(data_numeric, 3)

mod$centers %>% View()

data_full <- cbind(data, cluster = mod$cluster)
