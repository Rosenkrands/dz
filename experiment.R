library(dz)
set.seed(1)

# Variables
repetitions <- 1:2
inst <- test_instances$p7_chao
L <- tail(seq(20, 400, 20), 1)
k <- tail(c(2, 3, 4), 1)

variance_list <- lapply(repetitions, function(x) generate_variances(inst))
info_list <- lapply(repetitions, function(x) generate_information(inst))

arguments <- expand.grid(rep_id = repetitions, L = L, k = k) |>
  dplyr::mutate(L = L/k)

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('variance_list', 'info_list', 'inst', 'arguments'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

rslt_list <- pbapply::pblapply(1:nrow(arguments), function(i) {
  iter_arguments <- arguments[i, ]
  cat("\n")
  print(iter_arguments)
  error = F
  tryCatch(
    expr = {
      # invisible(capture.output(suppressMessages({
        p_inst <- prepare_instance(inst, variance_list[[i]], info_list[[i]])
        L <- iter_arguments$L
        k <- iter_arguments$k
        num_routes <- 100

        rb_clust <- rb_clustering(p_inst, L, k, num_routes, info_list[[i]])
        zones <- rb_clust$zones

        sr <- starting_routes(inst, zones, L)
        expected_score <- do.call(sum, sr$s_total)

        ur <- update_routes(sr, L, variance_list[[i]], info_list[[i]])
        realized_score <- do.call(sum, ur$s_total)
      # })))
    },
    error = function(e) {
      print(e); error <<- T
    }
  )

  if (error) {
    return(iter_arguments)
  } else {
    return(tibble::tibble(list(p_inst), list(rb_clust), list(sr), list(ur)))
  }
}#, cl = cl
)
closeAllConnections()

success <- lapply(rslt_list, function(x) "tbl_df" %in% class(x))
rslt <- dplyr::bind_rows(rslt_list[do.call(c, success)])
rslt_list[!do.call(c,success)]

# rslt |>
#   dplyr::mutate(ur_L_remaining = )

# lapply(rslt$`list(ur)`, function(x) x$L_remaining)[[1]]
# lapply(rslt$`list(ur)`, function(x) x$s_total)[[1]]
#
plot(rslt$`list(sr)`[[1]], inst)
plot(rslt$`list(ur)`[[1]], inst)
