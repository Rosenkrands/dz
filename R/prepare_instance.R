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
      unexpected = purrr::rbernoulli(1, p = p_unexpected),
      realized_score = score,
      expected_score = score
    )|>
    dplyr::ungroup()

  # Update realized score to account for information from unexpected events
  for (i in inst$points$id) { # we need to consider all nodes
    related_nodes <- which(info[i,] != 0) # find the nodes that are related
    for (j in related_nodes) { # update score
      inst$points$expected_score[j] <- inst$points$score[j] + inst$points$p_unexpected[i] * info[i,j]
      if (inst$points$unexpected[i]) {
        inst$points$realized_score[j] <- inst$points$score[j] + info[i,j]
      }
    }
  }

  # # update expected score
  # id_now = 1
  # related_nodes <- which(info[id_now,] != 0)
  # for (i in related_nodes) {
  #   inst$points$expected_score[i] <- inst$points$expected_score[i] - inst$points$p_unexpected[i] * info[i,j]
  #   if (inst$points$unexpected) {
  #     inst$points$expected_score[i] <- inst$points$expected_score[i] + info[i,j]
  #   }
  # }

  inst$points$expected_score[1] <- 0; inst$points$realized_score[1] <- 0
  inst$info <- info

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
      data = p_inst$points |> dplyr::filter(point_type == "intermediate", unexpected == F),
      ggplot2::aes(x, y, size = score, color = score)
    ) +
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "intermediate", unexpected == T),
      ggplot2::aes(x, y, size = score, color = score), shape = 1
    ) +
    # Draw a cross on intermediate nodes that have an unexpected observation
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "intermediate", unexpected == T),
      ggplot2::aes(x = x, y = y), shape = 4
    ) +
    # Draw the terminal node
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # Title, theme and legend adjustments
    # ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(size = "none") +
    ggplot2::coord_fixed()
}
