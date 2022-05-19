library(dz)
library(tidyverse)

results_direc <- "C:/Users/krose/Desktop/experiment results 0dot5"

load_results_files <- function(direc) {
  pbapply::pblapply(
    list.files(direc, full.names = T),
    function(x) readRDS(x)
  )
}

message("Loading results files...")
rslt <- do.call(bind_rows, load_results_files(results_direc)) |>
  select(-`list(ur)`) |>
  mutate(sr_score = sapply(rslt$`list(sr)`, function(x) do.call(sum, x$total_score)),
         k = sapply(rslt$`list(sr)`, function(x) length(x$zones)),
         L = sapply(rslt$`list(sr)`, function(x) x$L * length(x$zones)))

rslt <- rslt |>
  rename(p_inst = `list(p_inst)`,
         clust = `list(rb_clust)`,
         sr = `list(sr)`)

message("Finding the best zones with best starting routes...")
best <- rslt |>
  group_by(k, L) |>
  slice_max(order_by = sr_score, n = 1, with_ties = F)

message("Find alternative zones with the heuristic approach...")
heuristic_best <- best |>
  select(p_inst, k, L)

heuristic_clusters <- pbapply::pblapply(1:nrow(best), function(row_id) {
  inst <- best$`list(p_inst)`[[row_id]]; k = best$k[row_id]; L = best$L[row_id]; cluster_method = "greedy"; alpha = 1; eps = 0

  suppressMessages(
    clust_ls <- clustering(inst, k, L, eps = 0, variances = NULL, info = NULL, cluster_method = "local_search", alpha = 1)
  )

  return(clust_ls)
})
heuristic_best$clust <- heuristic_clusters

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('heuristic_best'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

heuristic_sr <- pbapply::pblapply(1:nrow(heuristic_best), function(row_id) {
  error = F
  tryCatch(
    expr = {
      sr <- starting_routes(
        inst = heuristic_best$p_inst[[row_id]],
        zones = heuristic_best$clust[[row_id]]$cl$zones,
        L = heuristic_best$L[row_id]
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

lapply(heuristic_sr, function(x) class[[2]])

tibble(heuristic_sr) |>
  rowwise() |>
  mutate(object = list(heuristic_sr[[2]])) |>
  ungroup() |>
  mutate(class = class(object)) |>
  group_by(class) |>
  summarise(n = n())
