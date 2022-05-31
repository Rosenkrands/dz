library(dz)
library(tidyverse)

combined_results <- readRDS("combined_results.rds") |>
  # filter(clustering_method != "heuristic") |>
  bind_rows(
    readRDS("combined_results_relevancy.rds") |> filter(clustering_method == "heuristic relevancy")
  ) |> ungroup()

combined_results <- combined_results |>
  mutate(mean_ur_score = sapply(ur_score, mean),
         median_ur_score = sapply(ur_score, median),
         mean_candidate_outside = sapply(candidate_outside, mean)) |>
  select(clustering_method, k, L, sr_score, mean_ur_score, median_ur_score, mean_candidate_outside, p_inst, clust, sr, scenarios, ur_score, candidate_outside) |>
  mutate(clustering_method = factor(clustering_method, levels = c("routing-based", "heuristic", "heuristic relevancy"),
                                    labels = c("RB", "HC", "HR")))

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

ggsave("./figures_for_report/starting_routes.pdf", width = 8, height = 3.5)

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

ggsave("./figures_for_report/updated_routes.pdf", width = 8, height = 3.5)

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

ggsave("./figures_for_report/updated_routes_w_more_points.pdf", width = 8, height = 3.5)

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
  mutate(name = factor(name,
                       levels = c("sr_score", "mean_ur_score"),
                       labels = c("Starting", "Updated")
                       )) |>
  ggplot(aes(x = k, y = value, color = clustering_method, linetype = name, group = paste(clustering_method, name))) +
  geom_point(aes(shape = clustering_method)) +
  geom_line() +
  facet_wrap(~clustering_method) +
  scale_x_continuous(n.breaks = 3) +
  theme_bw() + theme(legend.position = "top") +
  labs(x = "Number of agents", y = "Total score", linetype = "Route type", color = "Solution method", shape = "Solution method")

ggsave("./figures_for_report/uav_effect.pdf", width = 8, height = 3.5)

uav_effect_minmax <- combined_results |>
  filter(L/k == 100) |>
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
            max_ur = max(ur_score))

uav_effect_data |>
  mutate(`Number of agents` = factor(k)) |>
  ggplot(aes(x = `Number of agents`, group = paste(clustering_method))) +
  geom_point(aes(y = sr_score, color = clustering_method), size = .8) +
  geom_line(aes(y = sr_score, linetype = "Starting", color = clustering_method)) +
  # geom_point(data = uav_effect_minmax, aes(y = mean_ur), size = 1) +
  geom_line(data = uav_effect_minmax, aes(y = mean_ur, linetype = "Updated")) +
  geom_errorbar(data = uav_effect_minmax, aes(ymin = min_ur, ymax = max_ur), width = .2)+
  facet_wrap(~clustering_method) +
  # scale_x_continuous(n.breaks = 3) +
  theme_bw() + theme(legend.position = "top") +
  labs(x = "Number of agents", y = "Total score", linetype = "Route type", color = "Solution method", shape = "Solution method")

ggsave("./figures_for_report/uav_effect_w_error.pdf", width = 8, height = 3.5)


cowplot::plot_grid(
  plot(uav_effect_data$sr[[1]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[4]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[7]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[2]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[5]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[8]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[3]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[6]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$sr[[9]], inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  nrow = 3
)

ggsave("./figures_for_report/compare_clustering_methods_sr.png", width = 13, height = 16.5)

find_best_sc <- function(row_id) {
  sc_score <- sapply(
    uav_effect_data$scenarios[[row_id]],
    function(x) do.call(sum, x$total_realized_score)
  )
  which.max(sc_score)
}

sapply(c(1,4,7,2,5,8,3,6,9), find_best_sc)

# cowplot::plot_grid(
#   plot(uav_effect_data$scenarios[[1]][[31]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[4]][[16]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[7]][[16]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[2]][[4]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[5]][[28]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[8]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[3]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[6]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   plot(uav_effect_data$scenarios[[9]][[39]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
#   nrow = 3,
#   labels = "AUTO"
# )

cowplot::plot_grid(
  plot(uav_effect_data$scenarios[[1]][[31]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[2]][[4]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[3]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[4]][[16]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[5]][[28]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[6]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[7]][[16]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[8]][[1]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  plot(uav_effect_data$scenarios[[9]][[39]]$ur, inst = uav_effect_data$p_inst[[1]]) + theme(legend.position = "none"),
  nrow = 3,
  labels = "AUTO"
)

ggsave("./figures_for_report/compare_clustering_methods_ur.png", width = 13, height = 16.5)
