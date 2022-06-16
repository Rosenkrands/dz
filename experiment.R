library(dz)
# set.seed(1)
pbapply::pboptions(use_lb = T)

# Variables
top_percentile = .5

repetitions <- 1
inst <- test_instances$p7_chao
L <- seq(570, 600, 30)
k <- c(6)

# variances <- generate_variances(inst)
info <- generate_information(inst)
p_inst_list <- pbapply::pblapply(repetitions, function(x) prepare_instance(inst, info))

arguments <- expand.grid(rep_id = repetitions, L = L, k = k) |>
  dplyr::mutate(L = L/k)

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('p_inst_list', 'info', 'inst', 'arguments', 'top_percentile'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

rslt_list <- pbapply::pblapply(1:nrow(arguments), function(i) {
  iter_arguments <- arguments[i, ]
  cat("\n")
  print(iter_arguments)
  error = F
  tryCatch(
    expr = {
      # invisible(capture.output(suppressMessages({
        last_function_call <- "p_inst"
        objects <- list()
        p_inst <- p_inst_list[[iter_arguments$rep_id]]
        L <- iter_arguments$L
        k <- iter_arguments$k
        num_routes <- 1000

        last_function_call <- "rb_clustering"
        L_adj <- L + .25*(200 - L)
        max_tries = 3
        attempt_no = 1
        while (attempt_no <= max_tries) {
          rb_clust <- try(rb_clustering(p_inst, L = L_adj, k, num_routes, info, top_percentile = top_percentile, weigthed = F))
          if (class(rb_clust) == "try-error") attempt_no = attempt_no + 1 else break
        }
        if (class(rb_clust) == "try-error") stop(as.character(rb_clust))
        zones <- rb_clust$zones
        objects$rb_clust <- rb_clust

        last_function_call <- "starting_routes"
        sr <- starting_routes(inst, zones, L)
        sr_score <- do.call(sum, sr$total_score)
        objects$sr <- sr

        last_function_call <- "update_routes2"
        ur <- update_routes2(p_inst, zones, L, k, sr, info)
        ur_score <- do.call(sum, ur$total_score)
        ur_realized_score <- do.call(sum, ur$total_realized_score)
        objects$ur <- ur
      # })))
    },
    error = function(e) {
      print(e); err <<- e; error <<- T
    }
  )

  if (error) {
    return(
      list(
        "args" = iter_arguments,
        "function" = last_function_call,
        "error" = err,
        "objects" = objects
      )
    )
  } else {
    return(tibble::tibble(list(p_inst), list(rb_clust), list(sr), list(ur)))
  }
}, cl = cl
)
closeAllConnections()

success <- lapply(rslt_list, function(x) "tbl_df" %in% class(x))
rslt <- dplyr::bind_rows(rslt_list[do.call(c, success)])
failed <- rslt_list[!do.call(c, success)]

plot(rslt$`list(sr)`[[1]], inst = inst)

saveRDS(
  rslt,
  file = paste0("C:\\Users\\krose\\Desktop\\experiment_",sub("\\.", "dot", top_percentile),"_rslt_5UAV_500.RDS")
)
saveRDS(
  failed,
  file = paste0("C:\\Users\\krose\\Desktop\\experiment_",sub("\\.", "dot", top_percentile),"_failed.RDS")
)

# plot(failed[[1]]$objects$rb_clust)

# rslt |>
#   dplyr::mutate(ur_L_remaining = )

# lapply(rslt$`list(ur)`, function(x) x$L_remaining)[[1]]
# lapply(rslt$`list(ur)`, function(x) x$s_total)[[1]]
#
# plot(rslt$`list(sr)`[[32]], inst)
# plot(rslt$`list(ur)`[[32]], inst)

# Analyze results
results_direc <- "C:/Users/krose/Desktop/experiment results 5UAV"
direcs <- list.files(results_direc, full.names = F)

load_results_files <- function(direc) {
  results <- lapply(
    list.files(paste(results_direc, direc, sep = "/"), full.names = T),
    function(x) readRDS(x)
  )

  if (length(results) == 2) {
    names(results) <- c("failed", "results")
  } else {
    names(results <- c("results"))
  }

  return(results)
}

rslt <- lapply(direcs, load_results_files)
names(rslt) <- sub("dot", "\\.", direcs)

all_results <- do.call(
  dplyr::bind_rows,
  lapply(seq_along(rslt), function(x) {
    rslt[[x]]$results |> dplyr::mutate(top_percentile = names(rslt)[x])
  })
)

# Diagnose what things have failed
failed_error <- lapply(seq_along(rslt), function(x) {
  lapply(rslt[[x]]$failed, function(x) x$error)
})
failed_args <- lapply(seq_along(rslt), function(x) {
  lapply(rslt[[x]]$failed, function(x) x$args)
})

names(failed_error) <- names(rslt); names(failed_args) <- names(rslt)

failed_args <- lapply(failed_args, function(x) do.call(dplyr::bind_rows, x))
failed_error <- lapply(failed_error, function(x) do.call(c, lapply(x, as.character)))

all_failed <- do.call(
  dplyr::bind_rows,
  lapply(
    seq_along(failed_error),
    function(x) dplyr::bind_cols(
      tibble::tibble(top_percentile = names(failed_error)[[x]], error = failed_error[[x]]),
      failed_args[[x]]
    )
  )
)

(ar <- all_results |>
  dplyr::mutate(ur_realized_score = sapply(all_results$`list(ur)`, function(x) do.call(sum, x$total_realized_score)),
                ur_score = sapply(all_results$`list(ur)`, function(x) do.call(sum, x$total_score)),
                ur_candidate_outside = sapply(all_results$`list(ur)`, function(x) do.call(sum, x$candidate_outside)),
                sr_score = sapply(all_results$`list(sr)`, function(x) do.call(sum, x$total_score)),
                k = sapply(all_results$`list(ur)`, function(x) length(x$zones)),
                L = sapply(all_results$`list(ur)`, function(x) x$L * length(x$zones))))

library(tidyverse)
ar |>
  ggplot(aes(y = ur_score, x = factor(L), color = factor(k)), group = paste(k,top_percentile)) +
  geom_point(position = position_dodge(width = .5)) +
  facet_wrap(~top_percentile, labeller = "label_both", ncol = 5) +
  theme_bw()
