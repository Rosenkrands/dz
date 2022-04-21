#' Prepare an instance for use with clustering and routing algorithms
#'
#' Generates variances and information and joins the information on the original instance
#'
#' @param name The name of the instance to use, eligible names are those in `names(dz::test_instances)`
#' @param variances A list returned from the `generate_variances` function
#' @param info A list returned from the `generate_information` function
#'
#' @return A list, i.e. a "prepared" instance
#' @export
#'
prepare_instance <- function(inst, variances, info) {
  # For testing purposes:
  # inst <- test_instances$p7_chao; variances <- generate_variances(inst); info <- generate_information(inst)

  inst$points <- inst$points |>
    dplyr::left_join(variances, by = c("id")) |>
    dplyr::mutate(dplyr::across(.cols = c(score_variance, p_unexpected), ~ tidyr::replace_na(.x, 0))) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      realized_score = rnorm(1, mean = score, sd = sqrt(score_variance)),
      unexpected = purrr::rbernoulli(1, p = p_unexpected)
    )|>
    dplyr::ungroup()

  # TODO: update realized score to account for information from unexpected events...

  class(inst) <- "prepared_instance"
  return(inst)
}

# inst <- test_instances$p7_chao
# variances <- generate_variances(inst)
# info <- generate_information(inst)
#
# p_inst <- prepare_instance(
#   inst,
#   variances,
#   info
# )
#
# p_inst

#' Plot method for a prepared_instance
#'
#' Building on the plot method from the instance object, this function adds information about socre variance and whether an unexpected event occurs at a node.
#'
#' @param p_inst A object returned from the `prepare_instance` function
#'
#' @return A ggplot2 object
#' @export
#'
plot.prepared_instance <- function(p_inst) {
  # Instantiate the ggplot object
  ggplot2::ggplot() +
    # Draw the edges in the graph
    ggplot2::geom_segment(
      data = p_inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    # Draw the intermediate nodes
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, size = score, color = score, stroke = unexpected, alpha = score_variance)
    ) +
    # Draw a cross on intermediate nodes that have an unexpected observation
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "intermediate", unexpected == T),
      ggplot2::aes(x, y), shape = 4
    ) +
    # Draw the terminal node
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # Title, theme and legend adjustments
    ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(size = "none")
}
