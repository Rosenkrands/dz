# Variables
set.seed(1)

repetitions <- 1:10
inst <- test_instances$p7_chao
L <- tail(seq(20, 400, 20), 1)
k <- tail(c(2, 3, 4), 1)

variance_list <- lapply(1:10, function(x) generate_variances(inst))
info_list <- lapply(1:10, function(x) generate_information(inst))

arguments <- expand.grid(rep_id = repetitions, L = L, k = k) |>
  dplyr::mutate(L = L/k)

rslt_list <- lapply(1:nrow(arguments), function(i) {
  p_inst <- prepare_instance(inst, variance_list[[i]], info_list[[i]])
  L <- arguments$L[i]
  k <- arguments$k[i]
  num_routes <- 100

  rb_clust <- rb_clustering(p_inst, L, k, num_routes, info)
  zones <- rb_clust$zones

  sr <- starting_routes(inst, zones, L)
  expected_score <- do.call(sum, sr$s_total)

  ur <- update_routes(sr, L, variances, info)
  realized_score <- do.call(sum, ur$s_total)

  return(tibble::tibble(list(p_inst), list(rb_clust), list(sr), list(ur)))
})

dplyr::bind_rows(rslt_list)
