library(dz)
library(tidyverse)
pbapply::pboptions(use_lb = T)

results_direc <- "C:/Users/krose/Desktop/experiment results 0dot5"

load_results_files <- function(direc) {
  pbapply::pblapply(
    list.files(direc, full.names = T),
    function(x) readRDS(x)
  )
}

message("Loading results files...")
rslt <- do.call(bind_rows, load_results_files(results_direc)) |>
  select(-`list(ur)`)

rslt <- rslt |>
  mutate(k = sapply(rslt$`list(sr)`, function(x) length(x$zones)),
         L = sapply(rslt$`list(sr)`, function(x) x$L * length(x$zones))) |>
  rename(p_inst = `list(p_inst)`,
         clust = `list(rb_clust)`,
         sr = `list(sr)`)

message("Make new starting routes using the new method...")
num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('rslt'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

rslt_sr <- pbapply::pblapply(1:nrow(rslt), function(row_id) {
  error = F
  tryCatch(
    expr = {
      zones <- rslt$clust[[row_id]]$zones
      sr <- starting_routes(
        inst = rslt$p_inst[[row_id]],
        zones = zones,
        L = rslt$L[row_id]/length(zones)
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

rslt$sr <- lapply(rslt_sr, function(x) x[[2]])

saveRDS(rslt, "./rslt_new_sr.rds")
# rslt
