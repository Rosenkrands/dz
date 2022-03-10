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
clustering <- function(inst, k, cluster_method = c("greedy", "local_search")) {
  # For testing purposes:
  # inst = test_instances$p7_chao; k = 4; cluster_method = "greedy"

  # Save only the intermediate points for clustering
  in_points <- inst$points |> dplyr::filter(point_type == "intermediate")

  # Compute edges in delaunay triangulation
  tri <- (deldir::deldir(inst$points$x, inst$points$y))$delsgs

  # calculate distance matrix
  dst <- Rfast::Dist(
    inst$points |> dplyr::select(x,y),
    method = "euclidean"
  )

  greedy_clustering <- function() {
    # add the source node to each zone
    zones <- list()
    for (i in 1:k) {
      zones[[i]] <- c(1)
    }

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
          dplyr::distinct() |>
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
      list(
        "inst_points" = inst$points |>
          dplyr::left_join(
            tibble::tibble(zone = 1:k, id = zones) |>
              tidyr::unnest(cols = id),
            by = c("id" = "id")
          ),
        "zones" = zones
      )

    )
  }

  local_search_clustering <- function() {
    # Compute the initial clustering
    gclust <- greedy_clustering()

    # storing the points and zones
    inst$points <- gclust$inst_points
    zones <- gclust$zones

    cluster_eval <- function(zone) {
      # For testing purposes:
      # zone = c(1, 40, 42, 76, 22, 60, 14, 75, 100, 61, 12, 98, 24, 37, 23, 13, 63, 27, 91, 49, 85, 78, 67, 79, 36, 102)

      total_profit <- inst$points |>
        dplyr::filter(id %in% zone) |>
        dplyr::summarise(score = sum(score)) |>
        dplyr::pull(score)

      average_distance <- expand.grid(id1 = zone, id2 = zone) |>
        dplyr::rowwise() |>
        dplyr::mutate(dist = dst[id1, id2]) |>
        dplyr::ungroup() |>
        dplyr::filter(dist != 0) |>
        dplyr::summarise(dist = mean(dist)) |>
        dplyr::pull(dist)

      average_distance/total_profit
    }

    # define the operators for the local search

    # take a point from one cluster and add it to another
    insertion <- function() {
      #do.call(sum, lapply(zones, cluster_eval))

      # A candidate should be understood as moving the respective id to the respective zone
      candidates <- tri |>
        dplyr::select(ind1, ind2) |>
        dplyr::filter(
          ind1 != 1, ind2 != 1, ind1 != nrow(inst$points), ind2 != nrow(inst$points)
        ) |>
        dplyr::inner_join(inst$points |> dplyr::select(id, zone), by = c("ind1" = "id")) |>
        dplyr::inner_join(inst$points |> dplyr::select(id, zone), by = c("ind2" = "id")) |>
        dplyr::filter(zone.x != zone.y) |>
        tidyr::pivot_longer(cols = c(zone.x, zone.y), values_to = "zone_id") |>
        dplyr::mutate(id1 = ifelse(name == "zone.y", ind1, ind2),
                      id2 = ifelse(name == "zone.x", ind1, ind2)) |>
        dplyr::select(id = id1, zone_id)

      insert_eval <- function(id, zone_id) {
        # find where the point is coming from and remove it
        for (i in 1:k) {
          if (i == zone_id) next
          if (id %in% zones[[i]]) {
            zones[[i]] <- zones[[i]][zones[[i]] != id]
          }
        }

        # then insert it in the new zone
        zones[[zone_id]] <- append(
          zones[[zone_id]],
          id,
          after = length(zones[[zone_id]]) - 1
        )
      }
    }
  }

  inst$points <- switch(
    cluster_method,
    "greedy" = (greedy_clustering())$inst_points
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
plot.clustering <- function(clust, delaunay = T) {
  # For testing purposes:
  # clust <- clustering(inst = test_instances$p7_chao, k = 4, cluster_method = "greedy")

  p <- ggplot2::ggplot()

    # If either delaunay or voronoi is true we compute the triangulation
  if (delaunay) {
    tri <- deldir::deldir(clust$instance$points$x, clust$instance$points$y)
  }

  # Add delaunay edges
  if (delaunay) {
    p <- p +
      ggplot2::geom_segment(
        data = tri$delsgs,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      )
  }

  p +
    # Find the smallest polygon including all nodes in each zone
    # ggalt::geom_encircle(
    #   data = clust$instance$points,
    #   ggplot2::aes(x = x, y = y, group = zone, fill = as.character(zone)),
    #   s_shape = 1, expand = 0, alpha = 0.2
    # ) +
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
