#' Generate clusters based on an instance
#'
#' Given an instance, number of clusters and a clustering method the function assigns each node of the problem instance a zone.
#'
#' @param inst An object returned from the `instance` function
#' @param k The number of clusters
#' @param cluster_method The method with which to perform the clustering
#'
#' @return A list ...
#' @export
#'
clustering <- function(inst, k, cluster_method = "pam") {
  # For testing purposes:
  # inst = test_instances$p7_chao; k = 4; cluster_method = "pam"

  # Save only the intermediate points for clustering
  in_points <- inst$points |> dplyr::filter(point_type == "intermediate")

  if (cluster_method == "pam") {
    # Perform PAM clustering on the intermediate points and save the clustering vector
    assign <- (cluster::pam(
      x = in_points |> dplyr::select(x, y),
      k = k,
      metric = "euclidean"
    ))$clustering
  } else {
    stop(paste0("cluster_method ", cluster_method, " not implemented"))
  }

  # Add the clustering vector to the intermediate points
  in_points$zone <- assign

  # Join zone assignment on all points
  inst$points <- inst$points |>
    dplyr::left_join(in_points |> dplyr::select(id, zone), by = c("id" = "id"))

  structure(
    list(
      "instance" = inst,
      "k" = k,
      "cluster_method" = cluster_method,
      "in_points" = in_points
    ),
    class = "clustering"
  )
}

#' Plot method for a clustering object
#'
#' Visualizes the zones obtained from the specific clustering algorithm.
#'
#' @param clust A list returned from the `clustering` function
#'
#' @return A ggplot object
#' @export
#'
plot.clustering <- function(clust) {
  ggplot2::ggplot() +
    # Find the smallest polygon including all nodes in each zone
    ggalt::geom_encircle(
      data = clust$in_points,
      ggplot2::aes(x = x, y = y, group = zone, fill = as.character(zone)),
      s_shape = 1, expand = 0, alpha = 0.2
    ) +
    # Plot the intermediate node with color according to score
    ggplot2::geom_point(
      data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color = score, shape = point_type)
    ) +
    # Plot the terminal nodes
    ggplot2::geom_point(
      data = clust$instance$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # Add title, theme and adjustment of guides
    ggplot2::ggtitle(paste0("Instance: ", clust$instance$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none"
    )
}
