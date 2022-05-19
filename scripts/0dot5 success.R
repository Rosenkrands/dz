library(dz)
library(tidyverse)

# Analyze results
results_direc <- "C:/Users/krose/Desktop/experiment results 0dot5"

load_results_files <- function(direc) {
  results <- lapply(
    list.files(direc, full.names = T),
    function(x) readRDS(x)
  )

  return(results)
}

rslt <- do.call(bind_rows, load_results_files(results_direc)) |>
  select(-`list(ur)`)

rslt <- rslt |>
  mutate(sr_score = sapply(rslt$`list(sr)`, function(x) do.call(sum, x$total_score)),
         k = sapply(rslt$`list(sr)`, function(x) length(x$zones)),
         L = sapply(rslt$`list(sr)`, function(x) x$L * length(x$zones)))

# rslt |> group_by(k) |> summarise(n())

rslt |>
  ggplot(aes(y = sr_score, x = factor(L), color = factor(k), group = k)) +
  # geom_point(position = position_dodge(width = .5), alpha = .2) +
  geom_point(alpha = .2) +
  # geom_jitter(alpha = .2) +
  # geom_line(data = rslt |> group_by(L,k) |> mutate(sr_score = median(sr_score)),
  #           position = position_dodge(width = .5)) +
  geom_line(data = rslt |> group_by(L,k) |> mutate(sr_score = median(sr_score))) +
  theme_bw() +
  labs(x = "Total L across team", y = "Total score of starting routes", color = "Size of team")

## update_route scenario function
ur_scenario <- function(row_id) {
  # variables
  p_inst <- best$`list(p_inst)`[[row_id]]
  zones <- best$`list(rb_clust)`[[row_id]]$zones
  L <- best$L[row_id] / length(zones)
  k <- length(zones)
  sr <- best$`list(sr)`[[row_id]]
  info <- best$`list(p_inst)`[[row_id]]$info

  # update unexpected and realized score
  p_inst$points <- p_inst$points |>
    mutate(unexpected = purrr::rbernoulli(1, p = p_unexpected))

  for (i in p_inst$points$id) { # we need to consider all nodes
    related_nodes <- which(info[i,] != 0) # find the nodes that are related
    for (j in related_nodes) { # update score
      p_inst$points$expected_score[j] <- p_inst$points$score[j] + p_inst$points$p_unexpected[i] * info[i,j]
      if (p_inst$points$unexpected[i]) {
        p_inst$points$realized_score[j] <- p_inst$points$score[j] + info[i,j]
      }
    }
  }

  p_inst$points$expected_score[1] <- 0; p_inst$points$realized_score[1] <- 0

  # Run update_routes2
  ur <- update_routes2(p_inst, zones, L, k, sr, info)

  # construct the realized_score over time
  ur_scores <- lapply(seq_along(ur$routes), function(route_id){
    route_time_n_score <- function(id) {
      sub_route <- ur$routes[[route_id]][1:id]

      score <- sum(p_inst$points$realized_score[unique(sub_route)])
      L_used <- tryCatch(sum(p_inst$dst[embed(sub_route, 2)]), error = function(e) 0)

      tibble(L_used, score, route_id)
    }

    do.call(bind_rows, lapply(seq_along(ur$routes[[route_id]]), route_time_n_score))
  })

  ur_scores_w_L <- do.call(bind_rows, ur_scores) |>
    pivot_wider(id_cols = c(L_used), names_from = "route_id", values_from = "score") |>
    arrange(L_used) |>
    fill(-L_used) |>
    mutate(total_score = rowSums(across(-L_used))) |>
    select(L_used, total_score)

  # return results
  list(
    "ur" = ur,
    "total_realized_score" = ur$total_realized_score,
    "candidate_outside" = ur$candidate_outside,
    "ur_scores_w_L" = ur_scores_w_L
  )
}

# select the best zones in terms of starting routes for each zone
best <- rslt |>
  group_by(k, L) |>
  slice_max(order_by = sr_score, n = 1)

best_scenarios <- pbapply::pblapply(1:nrow(best), function(row_id) {
  reps = 1:30
  scenarios <- lapply(reps, function(x) ur_scenario(row_id))
  names(scenarios) <- reps
  return(scenarios)
})

best$scenarios <- best_scenarios

row_id <- 31

plot_data <- do.call(
  bind_rows,
  lapply(
    seq_along(best$scenarios[[row_id]]),
    function(x) best$scenarios[[row_id]][[x]]$ur_scores_w_L |> mutate(rep = names(best$scenarios[[row_id]])[x])
  )
)

## calculate difference from starting route score at that point
improved_routes <- best$`list(sr)`[[row_id]]$improved_routes
p_inst <- best$`list(p_inst)`[[row_id]]

sr_scores <- lapply(seq_along(improved_routes), function(route_id){
  route_time_n_score <- function(id) {
    sub_route <- improved_routes[[route_id]][1:id]

    score <- sum(p_inst$points$realized_score[unique(sub_route)])
    L_used <- tryCatch(sum(p_inst$dst[embed(sub_route, 2)]), error = function(e) 0)

    tibble(L_used, score, route_id)
  }

  do.call(bind_rows, lapply(seq_along(improved_routes[[route_id]]), route_time_n_score))
})

sr_scores_w_L <- do.call(bind_rows, sr_scores) |>
  pivot_wider(id_cols = c(L_used), names_from = "route_id", values_from = "score") |>
  arrange(L_used) |>
  fill(-L_used) |>
  mutate(total_score = rowSums(across(-L_used))) |>
  select(L_used, total_score)

plot_data$sr_total_score <- pbapply::pbsapply(plot_data$L_used, function(x) {
  sr_scores_w_L |> filter(L_used <= x) |> slice_max(total_score, n = 1, with_ties = F) |> pull(total_score)
})

ggplot(plot_data, aes(x = L_used)) +
  geom_line(aes(y = total_score - sr_total_score, linetype = "Updated route", group = rep)) +#, color = "darkgrey") +
  geom_line(aes(y = 0, linetype = "Starting route"), ) +
  theme_bw() +
  labs(x = "L", y = "Relative cumulative score")

# find the routes
plot(best$`list(sr)`[[row_id]], inst = best$`list(p_inst)`[[row_id]])
plot(best$scenarios[[row_id]]$`1`$ur, inst = best$`list(p_inst)`[[row_id]])
