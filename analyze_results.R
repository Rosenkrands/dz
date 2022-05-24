library(dz)
library(tidyverse)

combined_results <- readRDS("C:/Users/krose/Desktop/combined_results.rds")

combined_results <- combined_results |>
  mutate(mean_ur_score = sapply(ur_score, mean),
         median_ur_score = sapply(ur_score, median),
         mean_candidate_outside = sapply(candidate_outside, mean)) |>
  select(clustering_method, k, L, sr_score, mean_ur_score, median_ur_score, mean_candidate_outside, p_inst, clust, sr, scenarios, ur_score, candidate_outside)

ggplot(combined_results, aes(x = L, y = sr_score, color = clustering_method)) +
  geom_point(aes(shape = clustering_method)) +
  geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
  facet_wrap(~factor(k), labeller = label_both) +
  theme_bw()

ggplot(combined_results, aes(x = L, y = mean_ur_score, color = clustering_method)) +
  geom_point(aes(shape = clustering_method)) +
  geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
  facet_wrap(~factor(k), labeller = label_both) +
  theme_bw()

ggplot(combined_results, aes(x = L, y = mean_candidate_outside, color = clustering_method)) +
  geom_point(aes(shape = clustering_method)) +
  geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
  facet_wrap(~factor(k), labeller = label_both) +
  theme_bw()
