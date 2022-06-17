library(dz)
library(tidyverse)
set.seed(2)

inst = test_instances$p7_chao
variances = generate_variances(inst)
info = generate_information(inst)

L = 50

benchmark_function <- function(num_routes, top_percentile) {
  p_inst = prepare_instance(inst, variances, info)
  sapply(1:num_routes, function(x) {
    initial_route2(p_inst, L, info, top_percentile)$realized_score
  })
}

percentiles = c(.05, .10, .15, .20, .25, .30, .50, 1.00)

# set up of parallel computation
num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, c('inst', 'variances', 'L', 'info', 'benchmark_function'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

rslt <- pbapply::pblapply(
  rep(percentiles, 500),
  function(x) benchmark_function(num_routes = 1, top_percentile = x),
  cl = cl
)

names(rslt) <- rep(percentiles, 500)

tibble::tibble(name = rep(as.character(percentiles), 500), value = do.call(c, rslt)) |>
  ggplot2::ggplot() +
  ggplot2::geom_boxplot(ggplot2::aes(x = name, y = value), alpha = .2) +
  theme_bw() +
  # theme(legend.position = "")
  labs(x = "Top percentile", y = "Route score")

ggsave("./figures_for_report/top_percentile_tuning.pdf", width = 6, height = 2.5)
