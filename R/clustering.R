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
clustering <- function(inst, k, cluster_method = c("greedy_p")) {
  # For testing purposes:
  # inst = test_instances$p7_chao; k = 4; cluster_method = "greedy_p"

  # Save only the intermediate points for clustering
  in_points <- inst$points |> dplyr::filter(point_type == "intermediate")

  greedy_clustering <- function() {
    # Compute edges in delaunay triangulation
    tri <- (deldir::deldir(inst$points$x, inst$points$y))$delsgs

    # add the source node to each zone
    zones <- list()
    for (i in 1:k) {
      zones[[i]] <- c(1)
    }

    # calculate distance matrix
    dst <- Rfast::Dist(
      inst$points |> dplyr::select(x,y),
      method = "euclidean"
    )

    # keep track of unassigned points
    unassigned <- in_points
    while(nrow(unassigned) > 0) {
      cat("unassigned points: ", nrow(unassigned), "\n")
      for (i in 1:k) {
        cat("zone: ", i, "\n")
        # assign nearest point to each zone
        points_in_zone <- zones[[i]]

        # Find the delaunay neighbors
        nghbr <- tri |>
          dplyr::filter((ind1 %in% unassigned$id) | (ind2 %in% unassigned$id)) |>
          dplyr::filter(
            ((ind1 %in% points_in_zone) & !(ind2 %in% points_in_zone)) |
            ((ind2 %in% points_in_zone) & !(ind1 %in% points_in_zone))
          ) |>
          dplyr::select(ind1, ind2) |>
          tidyr::pivot_longer(cols = c(ind1, ind2), names_to = NULL, values_to = "id") |>
          dplyr::filter(!(id %in% points_in_zone)) |>
          dplyr::pull(id)

        # if there are no neighbors go to next iteration
        if (length(nghbr) < 1) next

        # find the closest point
        closest_points <- expand.grid(id1 = points_in_zone, id2 = nghbr) |>
          dplyr::rowwise() |>
          dplyr::mutate(dist = dst[id1, id2]) |>
          dplyr::ungroup() |>
          dplyr::slice_min(dist, n = 1) |>
          dplyr::pull(id2)

        # add to zone
        zones[[i]] <- append(zones[[i]], closest_points)

        # update unassigned
        unassigned <- unassigned |> dplyr::filter(!(id %in% closest_points))
      }
    }

    # add the sink node to each zone
    for (i in 1:k) {
      zones[[i]] <- append(zones[[i]], nrow(inst$points))
    }

    return(
      inst$points |>
        dplyr::left_join(
          tibble::tibble(zone = 1:k, id = zones) |>
            tidyr::unnest(cols = id),
          by = c("id" = "id")
        )
    )
  }

  inst$points <- switch(
    cluster_method,
    "greedy_p" = greedy_clustering()
  )

  structure(
    list(
      "instance" = inst,
      "k" = k,
      "cluster_method" = cluster_method
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
  # For testing purposes:
  # clust <- clustering(inst = test_instances$p7_chao, k = 4, cluster_method = "greedy_p")

  ggplot2::ggplot() +
    # Find the smallest polygon including all nodes in each zone
    ggalt::geom_encircle(
      data = clust$instance$points,
      ggplot2::aes(x = x, y = y, group = zone, fill = as.character(zone)),
      s_shape = 1, expand = 0, alpha = 0.2
    ) +
    # Plot the intermediate node with color according to score
    ggplot2::geom_point(
      data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color = as.character(zone), shape = point_type)
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
