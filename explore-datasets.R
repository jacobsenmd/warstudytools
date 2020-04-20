library(tidyverse)

episode.years<-readRDS("data/episode_years.rds")
episodes<-readRDS("data/episodes.rds")
conflicts<-readRDS("data/conflicts.rds")

# Plot histogram of episode duration
ggplot(episodes, aes(x=episode_duration)) + 
  geom_histogram() +
  labs(x="Duration (years)", 
       y="Episodes")
ggsave("out/hist-episode-duration.png")

# Plot histogram of conflict duration
ggplot(conflicts, aes(x=conflict_duration)) + 
  geom_histogram() +
  labs(x="Duration (years)", 
       y="Conflicts")
ggsave("out/hist-conflict-duration.png")

# Plot histogram of episode actor count
ggplot(episodes, aes(x=side_a_count + side_b_count)) + 
  geom_histogram() +
  labs(x="Primary actor count", 
       y="Episodes")
ggsave("out/hist-episode-actor-count.png")

# Plot histogram of conflict actor count
ggplot(conflicts, aes(x=side_a_count + side_b_count)) + 
  geom_histogram() +
  labs(x="Primary actor count", 
       y="Conflicts")
ggsave("out/hist-conflict-actor-count.png")

# Plot episode duration vs. actor count
ggplot(episodes, aes(x=side_a_count + side_b_count, y=episode_duration)) + 
  geom_point() +
  geom_smooth(span=1) +
  labs(x="Primary actor count",
       y="Episode duration (years)")
ggsave("out/episode-duration-actor-count.png")

# Plot conflict duration vs. actor count
ggplot(conflicts, aes(x=side_a_count + side_b_count, y=conflict_duration)) + 
  geom_point() +
  geom_smooth(span=1) +
  labs(x="Primary actor count",
       y="Conflict duration (years)")
ggsave("out/conflict-duration-actor-count.png")

episodes %>% 
  filter(episode_duration > 20)

episodes<-episodes %>% mutate(pri = side_a_count + side_b_count)
conflicts<-conflicts %>% mutate(pri = side_a_count + side_b_count)
summary(lm(episode_duration ~ pri, episodes))
summary(lm(conflict_duration ~ pri, conflicts))


# Demonstrations of how to navigate data ----------------------------------

# Show how to filter and sort data
acd %>%
  filter(location=="Iraq") %>%
  arrange(year) %>%
  View()