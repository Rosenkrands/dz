library(dz)
library(tidyverse)

# results_direc <- "C:/Users/krose/Desktop/experiment results 0dot5"
#
# load_results_files <- function(direc) {
#   pbapply::pblapply(
#     list.files(direc, full.names = T),
#     function(x) readRDS(x)
#   )
# }
#
# message("Loading results files...")
# rslt <- do.call(bind_rows, load_results_files(results_direc)) |>
#   select(-`list(ur)`)
#
# rslt <- rslt |>
#   mutate(sr_score = sapply(rslt$`list(sr)`, function(x) do.call(sum, x$total_score)),
#          k = sapply(rslt$`list(sr)`, function(x) length(x$zones)),
#          L = sapply(rslt$`list(sr)`, function(x) x$L * length(x$zones))) |>
#   rename(p_inst = `list(p_inst)`,
#          clust = `list(rb_clust)`,
#          sr = `list(sr)`)

rslt <- readRDS("./rslt_new_sr.rds")
rslt <- rslt |>
  mutate(sr_score = sapply(rslt$sr, function(x) do.call(sum, x$total_score)))

message("Finding the best zones with best starting routes...")
best <- rslt |>
  group_by(k, L) |>
  slice_max(order_by = sr_score, n = 1, with_ties = F)

message("Find alternative zones with the heuristic approach...")
heuristic_best <- best |>
  select(p_inst, k, L)

heuristic_clusters <- pbapply::pblapply(1:nrow(best), function(row_id) {
  inst <- best$p_inst[[row_id]]; k = best$k[row_id]; L = best$L[row_id]/k; info = best$p_inst[[row_id]]$info

  suppressMessages(
    clust_ls <- clustering(inst, k, L = L + .25*(200 - L), eps = 0, variances = NULL, info, cluster_method = "local_search", alpha = 1)
  )

  return(clust_ls)
})
heuristic_best$clust <- heuristic_clusters

message("Create starting routes for the heuristic clusters...")
num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('heuristic_best'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

heuristic_sr <- pbapply::pblapply(1:nrow(heuristic_best), function(row_id) {
  error = F
  tryCatch(
    expr = {
      zones <- heuristic_best$clust[[row_id]]$cl$zones
      sr <- starting_routes(
        inst = heuristic_best$p_inst[[row_id]],
        zones = zones,
        L = heuristic_best$L[row_id]/length(zones)
      )
    },
    error = function(e) {
      print(e); err <<- e; error <<- T
    }
  )

  if (error) {
    return(list(row_id, err))
  } else {
    return(list(row_id, sr))
  }
}, cl = cl
)

heuristic_best$sr <- lapply(heuristic_sr, function(x) x[[2]])
heuristic_best <- heuristic_best |>
  ungroup() |>
  mutate(sr_score = sapply(heuristic_best$sr, function(x) do.call(sum, x$total_score))) |>
  select(p_inst, clust, sr, sr_score, k, L)

# best_sr <- pbapply::pblapply(1:nrow(best), function(row_id) {
#   error = F
#   tryCatch(
#     expr = {
#       zones <- best$clust[[row_id]]$zones
#       sr <- starting_routes(
#         inst = best$p_inst[[row_id]],
#         zones = zones,
#         L = best$L[row_id]/length(zones)
#       )
#     },
#     error = function(e) {
#       print(e); err <<- e; error <<- T
#     }
#   )
#
#   if (error) {
#     return(list(row_id, err))
#   } else {
#     return(list(row_id, sr))
#   }
# }, cl = cl
# )
#
# best$sr <- lapply(best_sr, function(x) x[[2]])

message("Combining heuristic and routing-based into one...")
combined_rslt <- bind_rows(
  best |> mutate(clustering_method = "routing-based"),
  heuristic_best |> mutate(clustering_method = "heuristic")
)

message("Simulating scenarios...")
ur_scenario <- function(row_id) {
  # variables
  p_inst <- combined_rslt$p_inst[[row_id]]
  if (class(combined_rslt$clust[[row_id]]) == "rb_clustering") {
    zones <- combined_rslt$clust[[row_id]]$zones
  } else {
    zones <- combined_rslt$clust[[row_id]]$cl$zones
  }
  L <- combined_rslt$L[row_id] / length(zones)
  k <- length(zones)
  sr <- combined_rslt$sr[[row_id]]
  info <- combined_rslt$p_inst[[row_id]]$info

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

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('combined_rslt', 'ur_scenario'))
invisible(parallel::clusterEvalQ(cl, {library(dz); library(tidyverse)}))

combined_scenarios <- pbapply::pblapply(1:nrow(combined_rslt), function(row_id) {
  reps = 1:50
  scenarios <- lapply(reps, function(x) ur_scenario(row_id))
  names(scenarios) <- reps
  return(scenarios)
}, cl = cl)

combined_rslt$scenarios <- combined_scenarios

message("Clean and augment the combined results dataset...")
# combined_rslt$scenarios[[1]][[1]]

combined_rslt$ur_score <- lapply(
  combined_rslt$scenarios,
  function(i) sapply(i, function(j) do.call(sum, j$total_realized_score))
)

combined_rslt$candidate_outside <- lapply(
  combined_rslt$scenarios,
  function(i) sapply(i, function(j) do.call(sum, j$candidate_outside))
)

message("Saving combined results...")
saveRDS(combined_rslt, file = "C:/Users/krose/Desktop/combined_results.rds")
