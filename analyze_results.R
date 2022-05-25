library(dz)
library(tidyverse)

combined_results <- readRDS("combined_results.rds")

combined_results <- combined_results |>
  mutate(mean_ur_score = sapply(ur_score, mean),
         median_ur_score = sapply(ur_score, median),
         mean_candidate_outside = sapply(candidate_outside, mean)) |>
  select(clustering_method, k, L, sr_score, mean_ur_score, median_ur_score, mean_candidate_outside, p_inst, clust, sr, scenarios, ur_score, candidate_outside)

# Comparison of clustering method by sr_score
combined_results |>
  mutate(`Number of agents` = factor(k)) |>
  ggplot(aes(x = L, y = sr_score, color = clustering_method)) +
    geom_point(aes(shape = clustering_method)) +
    geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
    facet_wrap(~`Number of agents`, labeller = label_both) +
    theme_bw() + labs(x = "Total L across team",
                      y = "Total score of starting routes",
                      color = "Solution method",
                      linetype = "Solution method",
                      shape = "Solution method") +
    theme(legend.position = "top")

ggsave("./figures_for_report/starting_routes.pdf", width = 8, height = 3)

# Comparison of clustering method by mean_ur_score
combined_results |>
  mutate(`Number of agents` = factor(k)) |>
  ggplot(aes(x = L, y = mean_ur_score, color = clustering_method)) +
    geom_point(aes(shape = clustering_method)) +
    geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
    facet_wrap(~`Number of agents`, labeller = label_both) +
    theme_bw() + labs(x = "Total L across team",
                      y = "Total mean score of updated routes",
                      color = "Solution method",
                      linetype = "Solution method",
                      shape = "Solution method") +
    theme(legend.position = "top")

ggsave("./figures_for_report/updated_routes.pdf", width = 8, height = 3)

# Comparison of clustering method by mean_ur_score
combined_results |>
  mutate(`Number of agents` = factor(k)) |>
  rowwise() |>
  mutate(
    ur_score = list(sapply(scenarios, function(x) do.call(sum, x$total_realized_score)))
  ) |>
  unnest(cols = ur_score) |>
  ungroup() |>
  group_by(clustering_method, `Number of agents`, L) |>
  summarise(mean_ur = mean(ur_score),
            min_ur = min(ur_score),
            max_ur = max(ur_score)) |>
  ggplot(aes(x = L, color = clustering_method)) +
  # geom_point(aes(y = mean_ur), size = 1) +
  geom_errorbar(aes(ymin = min_ur, ymax = max_ur), width = 6)+#, position = position_dodge(.5)) +
  # geom_line(aes(y = mean_ur, linetype = clustering_method, group = paste(`Number of agents`, clustering_method))) +
  facet_wrap(~`Number of agents`, labeller = label_both) +
  theme_bw() + labs(x = "Total L across team",
                    y = "Total mean score of updated routes",
                    color = "Solution method",
                    linetype = "Solution method",
                    shape = "Solution method") +
  theme(legend.position = "top")

ggsave("./figures_for_report/updated_routes_w_more_points.pdf", width = 8, height = 4)

# Regression test for difference in updated route score
ur_data <- combined_results |>
  ungroup() |>
  select(clustering_method, k, L, scenarios) |>
  rowwise() |>
  mutate(
    ur_score = list(sapply(scenarios, function(x) do.call(sum, x$total_realized_score)))
  ) |>
  unnest(cols = ur_score) |>
  select(-scenarios)

summary(lm(ur_score ~ clustering_method, data = ur_data |> filter(k == 2)))
summary(lm(ur_score ~ clustering_method, data = ur_data |> filter(k == 3)))
summary(lm(ur_score ~ clustering_method, data = ur_data |> filter(k == 3, L < 300)))
summary(lm(ur_score ~ clustering_method, data = ur_data |> filter(k == 4)))
summary(lm(ur_score ~ clustering_method, data = ur_data |> filter(k == 4, L < 300)))

# Regression test for difference in candidates outside
cu_data <- combined_results |>
  ungroup() |>
  select(clustering_method, k, L, scenarios) |>
  rowwise() |>
  mutate(
    cu = list(sapply(scenarios, function(x) do.call(sum, x$candidate_outside)))
  ) |>
  unnest(cols = cu) |>
  select(-scenarios)

summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 2)))
summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 3)))
summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 4)))
summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 2, L <= 300)))
summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 3, L <= 300)))
summary(lm(cu ~ clustering_method, data = cu_data |> filter(k == 4, L <= 300)))
summary(lm(cu ~ clustering_method, data = cu_data))

# Comparison of clustering method by mean number of candidates outside the zone
ggplot(combined_results, aes(x = L, y = mean_candidate_outside, color = clustering_method)) +
  geom_point(aes(shape = clustering_method)) +
  geom_line(aes(linetype = clustering_method, group = paste(factor(k), clustering_method))) +
  facet_wrap(~factor(k), labeller = label_both) +
  theme_bw()

# Effect of adding another UAV
uav_effect_data <- combined_results |>
  filter(L/k == 100)

uav_effect_data |>
  pivot_longer(cols = c(sr_score, mean_ur_score)) |>
  ggplot(aes(x = k, y = value, color = clustering_method, linetype = name, group = paste(clustering_method, name))) +
  geom_point(aes(shape = factor(k))) +
  geom_line() +
  facet_wrap(~clustering_method) +
  scale_x_continuous(n.breaks = 3) +
  theme_bw()

cowplot::plot_grid(
  plot(uav_effect_data$sr[[1]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[4]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[2]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[5]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[3]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[6]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  nrow = 3
)
ggsave("./figures_for_report/compare_clustering_methods_sr.png", width = 9, height = 13.5)
