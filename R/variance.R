#' Generate a set of variances for the stochastic profits
#'
#' Given an instance the function will generate a variance number for each individual node.
#'
#' @param inst A list returned from the `instance` function
#' @param distribution A distribution to use, for only "discrete_uniform" is available
#' @param bounds Bounds to use for the distribution in the uniform case
#'
#' @return a tibble with `id` and `score_variance`
#' @export
#'
generate_variances <- function(inst, distribution = "discrete_uniform", bounds = c("min" = 10, "max" = 40)) {
  # For testing purposes:
  # inst <- test_instances$p7_chao; distribution = "discrete_uniform"; bounds = c("min" = 10, "max" = 40)

  ids <- inst$points |>
    dplyr::filter(point_type == "intermediate") |>
    dplyr::select(id)

  sds <- round(runif(nrow(ids), min = bounds["min"], max = bounds["max"]))

  ids$score_variance <- sds

  return(ids)
}
