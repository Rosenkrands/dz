combined_results_new_sr <- readRDS("combined_results_new_sr.rds") |>
  # filter(clustering_method != "heuristic") |>
  bind_rows(
    readRDS("combined_results_new_sr_relevancy.rds") |> filter(clustering_method == "heuristic relevancy")
  ) |> ungroup()

combined_results_new_sr <- combined_results_new_sr |>
  mutate(mean_ur_score = sapply(ur_score, mean),
         median_ur_score = sapply(ur_score, median),
         mean_candidate_outside = sapply(candidate_outside, mean)) |>
  select(clustering_method, k, L, sr_score, mean_ur_score, median_ur_score, mean_candidate_outside, p_inst, clust, sr, scenarios, ur_score, candidate_outside) |>
  mutate(clustering_method = factor(clustering_method, levels = c("routing-based", "heuristic", "heuristic relevancy"),
                                    labels = c("RB", "HC", "HR")))

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

bind_rows(
  combined_results |> mutate(sr_method = "Standard"),
  combined_results_new_sr |> mutate(sr_method = "Reserved range")
) |>
  group_by(L, sr_method, clustering_method) |>
  summarise(mean_ur_score = mean(mean_ur_score)) |>
  # filter(clustering_method == "HC") |>
  ggplot(aes(x = L, y = mean_ur_score, color = sr_method)) +
  geom_point() +
  geom_line() +
  facet_wrap(~clustering_method) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(x = "Total L across team", y = "Total mean score of updated routes", color = "Starting route method")

ggsave("./figures_for_report/reserved_range_across_agents.pdf", width = 8, height = 3.5)
