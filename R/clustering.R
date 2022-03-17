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
  # inst = test_instances$p7_chao; k = 4; cluster_method = "local_search"

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

    # THE FOLLOWING COULD ALSO BE DONE IN CPP IF WORTH THE TIME
    # keep track of unassigned points
    unassigned <- in_points
    while(nrow(unassigned) > 0) {
      for (i in 1:k) {
        cat("unassigned points:", nrow(unassigned), "\r")
        # cat("zone: ", i, "\n")
        # assign nearest point to each zone
        points_in_zone <- zones[[i]]

        # if (length(zones[[4]]) == 16) stop("we are here!")

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
          dplyr::slice_min(dist, n = 1, with_ties = F) |>
          dplyr::pull(id2)

        # add to zone
        zones[[i]] <- append(zones[[i]], closest_points)

        # update unassigned
        unassigned <- unassigned |> dplyr::filter(!(id %in% closest_points))
      }
    }
    cat("unassigned points: 0\nall done!")

    # add the sink node to each zone
    for (i in 1:k) {
      zones[[i]] <- append(zones[[i]], nrow(inst$points))
    }

    return(
      list(
        "inst_points" = inst$points |>
          dplyr::left_join(
            tibble::tibble(zone = 1:k, id = zones) |>
              tidyr::unnest(cols = id) |>
              dplyr::filter(id != 1, id != inst$n),
            by = c("id" = "id")
          ),
        "zones" = zones
      )
    )
  }

  local_search_clustering <- function() {
    message("Performing the initial greedy clustering")
    gclust <- greedy_clustering()

    # storing the points and zones
    inst$points <- gclust$inst_points
    zones <- gclust$zones

    # wrapper for the cluster eval function to restrict to a single argument
    cluster_eval <- function(zone) {
      # cpp version of the calculation
      cluster_eval_cpp(
        zone,
        inst = inst$points |> dplyr::select(id,x,y,score) |> data.matrix()
      )
    }

    # Operators for the local search (for now there is only insertion)
    # take a point from one cluster and add it to another
    insertion <- function(zones) {

      # Update zones for generating the candidates
      inst_temp <- inst$points |>
        dplyr::select(id) |>
        dplyr::left_join(
          tibble::tibble(zone = 1:k, id = zones) |>
            tidyr::unnest(cols = id) |>
            dplyr::filter(id != 1, id != inst$n),
          by = c("id" = "id")
        )

      # A candidate should be understood as moving the respective id to the respective zone
      candidates <- tri |>
        dplyr::select(ind1, ind2) |>
        dplyr::filter(
          ind1 != 1, ind2 != 1, ind1 != nrow(inst$points), ind2 != nrow(inst$points)
        ) |>
        dplyr::inner_join(inst_temp, by = c("ind1" = "id")) |>
        dplyr::inner_join(inst_temp, by = c("ind2" = "id")) |>
        dplyr::filter(zone.x != zone.y) |>
        tidyr::pivot_longer(cols = c(zone.x, zone.y), values_to = "zone_id") |>
        dplyr::mutate(id1 = ifelse(name == "zone.y", ind1, ind2),
                      id2 = ifelse(name == "zone.x", ind1, ind2)) |>
        dplyr::select(id = id1, zone_id) |>
        dplyr::distinct()

      # Calculate the resulting objective of performing the insertion
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

        do.call(sum, lapply(zones, cluster_eval))
      }

      best_insert <- NA
      best_insert_obj <- Inf
      cat("\n")
      for (i in 1:nrow(candidates)) {
        cat("candidates left:", nrow(candidates) - i, "\r")
        id <- candidates[i,] |> dplyr::pull(id)
        zone_id <- candidates[i,] |> dplyr::pull(zone_id)

        candidate_obj <- insert_eval(id, zone_id)
        if (candidate_obj < best_insert_obj) {
          best_insert_obj <- candidate_obj
          best_insert <- candidates[i,]
        }
      }

      # best_insert <- candidates |>
      #   dplyr::rowwise() |>
      #   dplyr::mutate(objective = insert_eval(id, zone_id)) |>
      #   dplyr::ungroup() |>
      #   dplyr::slice_min(objective, n = 1, with_ties = F)

      # print(best_insert)

      # find where the point is coming from and remove it
      for (i in 1:k) {
        # message("looking for best_insert$id")
        if (i == best_insert$zone_id) next
        if (best_insert$id %in% zones[[i]]) {
          # message("found the origin of best_insert$id")
          zones[[i]] <- zones[[i]][zones[[i]] != best_insert$id]
          break
        }
      }

      # then insert it in the new zone
      zones[[best_insert$zone_id]] <- append(
        zones[[best_insert$zone_id]],
        best_insert$id,
        after = length(zones[[best_insert$zone_id]]) - 1
      )

      zones
    }

    # local search part
    message("Starting the local search")
    iter_max = 100

    initial_obj <- do.call(sum, lapply(zones, cluster_eval))
    cat("Inital objective is:", initial_obj, "\n")

    # save convergence data in vector
    ls_obj <- numeric(length = iter_max + 1)
    ls_obj[1] <- initial_obj

    # save zones for animation
    zones_list <- list()
    zones_list[[1]] <- zones

    bks_obj <- initial_obj

    iter = 0
    while(T) {
      iter = iter + 1
      new_zones <- insertion(zones)
      new_obj <- do.call(sum, lapply(new_zones, cluster_eval))

      if (new_obj < bks_obj) {
        zones <- new_zones
        bks_obj <- new_obj

        # saving data for the convergence plot
        ls_obj[iter + 1] <- new_obj

        # saving data for the zone animation
        zones_list[[iter + 1]] <- zones

        cat("objective:", bks_obj, "\titeration:", iter)
      } else {
        ls_obj[iter + 1] <- ls_obj[iter]
        zones_list[[iter + 1]] <- zones_list[[iter]]
        cat("\nlocal search conclude with bks:", bks_obj)
        break
      }

      if (iter == iter_max) {
        cat("\nreached maximum number of iterations\n")
        break
      }
    }

    # Adjust iter_max if local search concluded
    iter_max <- iter

    return(
      list(
        "inst_points" = inst$points |>
          dplyr::select(id, point_type, x, y, score) |>
          dplyr::left_join(
            tibble::tibble(zone = 1:k, id = zones) |>
              tidyr::unnest(cols = id) |>
              dplyr::filter(id != 1, id != inst$n),
            by = c("id" = "id")
          ),
        "zones" = zones,
        "plot_data" = list("obj" = ls_obj, "zones" = zones_list, "iter_max" = iter_max)
      )
    )
  }

  cl <- switch(
    cluster_method,
    "greedy" = greedy_clustering(),
    "local_search" = local_search_clustering()
  )

  inst$points <- cl$inst_points

  same_zone_edges <- tri |>
    dplyr::left_join(
      inst$points |> dplyr::select(id, zone),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      inst$points |> dplyr::select(id, zone),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (is.na(zone.x) | is.na(zone.y))) |>
    dplyr::mutate(zone = ifelse(is.na(zone.x), zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  structure(
    list(
      "instance" = inst,
      "k" = k,
      "cluster_method" = cluster_method,
      "same_zone_edges" = same_zone_edges,
      "plot_data" = cl$plot_data
    ),
    class = "clustering"
  )
}

#' Plot method for a clustering object
#'
#' Visualizes the zones obtained from the specific clustering algorithm.
#'
#' @param clust A list returned from the `clustering` function
#' @param delaunay Whether to show delaunay edges on the plot
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
    delsgs_same_zone <- clust$same_zone_edges |>
      dplyr::select(x1,y1,x2,y2) |>
      dplyr::distinct()
  }

  # Add delaunay edges
  if (delaunay) {
    p <- p +
      ggplot2::geom_segment(
        data = delsgs_same_zone,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      )
  }

  p +
    # Plot the intermediate node with color according to score
    ggplot2::geom_point(
      data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color = as.character(zone))
    ) +
    # ggplot2::geom_point(
    #   data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
    #   ggplot2::aes(x, y, color = as.character(zone), shape = point_type)
    # ) +
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
      fill = "none",
      color = "none"
    )
}

#' Generate animation of the local search clustering approach
#'
#' @param clust A list returned by clustering function with cluster_method = "local_search"
#' @param filename The filename of the gif to be saved
#'
#' @return Nothing, saves a .gif file in the current workdir
#' @export
#'
animate_local_search <- function(clust, filename = "animation.gif") {
  # For testing purposes:
  # clust <- clust_ls

  # function to plot objective from specific iteration
  plot_iter_convergence <- function(iter) {
    y_min <- min(clust$plot_data$obj[1:clust$plot_data$iter_max + 1])
    y_max <- max(clust$plot_data$obj[1:clust$plot_data$iter_max + 1])

    tibble::tibble(x = 0:(clust$plot_data$iter_max), y = clust$plot_data$obj[1:(clust$plot_data$iter_max+1)]) |>
      # dplyr::mutate(y = y/y[1]*100) |>
      dplyr::filter(x < iter + 1) |>
      ggplot2::ggplot(ggplot2::aes(x, y, group = 1)) +
      ggplot2::geom_line(color = "black") +
      # ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(limits = c(0,clust$plot_data$iter_max)) +
      ggplot2::scale_y_continuous(limits = c(y_min, y_max)) +
      ggplot2::labs(
        x = "Iteration",
        y = "Objective",
        title = "Convergence of local search"
      )
  }

  # function to plot zone from specific iteration
  plot_iter_zones <- function(iter, delaunay = T) {
    zones_list <- clust$plot_data$zones

    plot_points <- clust$instance$points |>
      dplyr::select(-zone) |>
      dplyr::left_join(
        tibble::tibble(zone = 1:clust$k, id = zones_list[[iter]]) |>
          tidyr::unnest(cols = id),
        by = c("id" = "id")
      )

    p <- ggplot2::ggplot()

    # If either delaunay or voronoi is true we compute the triangulation
    if (delaunay) {
      delsgs_same_zone <- clust$same_zone_edges |>
        dplyr::select(x1,y1,x2,y2) |>
        dplyr::distinct()
    }

    # Add delaunay edges
    if (delaunay) {
      p <- p +
        ggplot2::geom_segment(
          data = delsgs_same_zone,
          ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
          color = ggplot2::alpha("black", 0.3), linetype = "dashed"
        )
    }

    p +
      # Plot the intermediate node with color according to score
      ggplot2::geom_point(
        data = plot_points |> dplyr::filter(point_type == "intermediate"),
        ggplot2::aes(x, y, color = as.character(zone), shape = point_type)
      ) +
      # Plot the terminal nodes
      ggplot2::geom_point(
        data = plot_points |> dplyr::filter(point_type == "terminal"),
        ggplot2::aes(x, y), color = "red", shape = 17
      ) +
      # Add title, theme and adjustment of guides
      ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
      ggplot2::theme_bw() +
      ggplot2::guides(
        shape = "none",
        fill = "none",
        color = "none"
      )
  }

  # animation function that prints individual frames
  local_search_animation <- function() {
    for (i in 1:clust$plot_data$iter_max + 1) {
      cat("iteration", i - 1, "of", clust$plot_data$iter_max, "\r")
      print(
        cowplot::plot_grid(
          plot_iter_convergence(iter = i - 1),
          plot_iter_zones(iter = i, delaunay = T)
        )
      )
    }
    cat("\n")
  }

  # generate the gif
  animation::saveGIF(
    local_search_animation(),
    movie.name = filename,
    interval = .1,
    ani.width = 1920,
    ani.height = 1080,
    ani.res = 250
  )
}
