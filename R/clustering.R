#' Generate clusters based on an instance
#'
#' Given an instance, number of clusters and a clustering method the function assigns each node of the problem instance a zone.
#'
#' @param inst An object returned from the `instance` function
#' @param k The number of clusters
#' @param L The length constraint for each agent
#' @param eps A sensitivity parameter for the preprocessing of the instance with regards to excluding points that are too far away.
#' @param variances
#' @param info
#' @param cluster_method The method with which to perform the clustering
#' @param alpha weight between mean distance/total profit and relevancy
#'
#' @return A list ...
#' @export
#'
clustering <- function(inst, k, L, eps, variances, info, cluster_method = c("greedy", "local_search"), alpha = 1) {
  # For testing purposes:
  # inst = test_instances$p7_chao; k = 5; L = 40; eps = 0; cluster_method = "local_search"; variances = generate_variances(inst); alpha = 0; info <- generate_information(inst, r = 100)

  inst$points <- inst$points |>
    dplyr::left_join(variances, by = c("id")) # Join variances on points tibble

  g <- inst$g
  dst <- inst$dst

  # Preprocessing the instance based on the length constraint
  inst$points$in_range <- dst[1,] < L-eps

  # Save only the intermediate points that are in range for clustering
  in_points <- inst$points |> dplyr::filter(point_type == "intermediate", in_range)

  greedy_clustering <- function() {
    # add the source node to each zone
    zones <- list()
    for (i in 1:k) {
      zones[[i]] <- c(1)
    }

    # keep track of unassigned points
    unassigned <- in_points
    while(nrow(unassigned) > 0) {
      for (i in 1:k) {
        message("unassigned points: ", nrow(unassigned))
        # message("zone: ", i, "\n")
        # assign nearest point to each zone
        points_in_zone <- zones[[i]]

        # print("finding the neighborhood")
        # Find the delaunay neighbors
        nghbr <- do.call(
          c,
          lapply(
            igraph::neighborhood(g, order = 1, nodes = points_in_zone),
            as.vector
          )
        ) |> unique() # gather the neighborhood for each point in the zone, concatenate and find the distinct nodes

        nghbr <- nghbr[nghbr %in% unassigned$id] # restrict neighborhood to unly unassigned nodes

        # if there are no neighbors go to next iteration
        if (length(nghbr) < 1) next

        # Lookup distances in the distance matrix, while removing loops (rows are neighbors, columns are points in zone)
        dst_temp <- dst[nghbr, points_in_zone] |> as.matrix()
        if (length(nghbr) == 1) dst_temp <- t(dst_temp) # If there is only one neighbor and multiple points in zone, rows and columns are switched so we transpose to keep rows as neighbors and columns as points in zone

        # return the first minimum distance and add to zone
        # first minimum distance (first column in neighbors, second column is points in zone)
        closest_point <- nghbr[arrayInd(which.min(dst_temp), dim(dst_temp))[,1]]
        # print(paste0("closest point is ", closest_point))
        if(is.na(closest_point)) stop("closest point is NA")

        zones[[i]] <- append(
          zones[[i]],
          closest_point
        )

        # update unassigned
        unassigned <- unassigned |> dplyr::filter(id != closest_point)
      }
    }
    # message("unassigned points: 0\nall done!\n")

    return(
      list(
        "inst_points" = inst$points |>
          dplyr::left_join(
            tibble::tibble(zone = 1:k, id = zones) |>
              tidyr::unnest(cols = id) |>
              dplyr::filter(id != 1),
            by = c("id")
          ) |>
          dplyr::mutate(zone = ifelse(id == 1, 0, zone)),
        "zones" = zones
      )
    )
  }

  local_search_clustering <- function() {
    message("Performing the initial greedy clustering")
    gclust <- greedy_clustering()

    # storing the points and zones
    # inst$points <- gclust$inst_points
    in_points <- in_points |>
      dplyr::left_join(
        gclust$inst_points |>
          dplyr::select(id, zone),
        by = c("id")
      )
    zones <- gclust$zones

    avg_dist_profit <- function(zone) {
      dst_temp <- dst[zone, zone] # subset the distance matrix (of shortest paths) to only nodes in the zone
      avg_dist <- mean(dst_temp[lower.tri(dst_temp, diag = F)]) # dst_temp is symmetric so we only need the lower triange (or equivalently upper) not including the diagonal (of all zeroes corresponding to all loop edges)
      if (is.na(avg_dist)) avg_dist <- Inf
      total_profit <- sum(inst$points$score[zone], na.rm = T) # get the total profit from the instance table
      total_variance <- sum(inst$points$score_variance[zone], na.rm = T)

      p <- .05
      q <- qnorm(p, mean = total_profit, sd = sqrt(total_variance))

      return(avg_dist/total_profit)
    }

    relevancy <- function(zone) {
      # find the absolute correlation between points in the zone
      # other_zone <- in_points$id[-zone]
      other_zone <- intersect(
        inst$points$id[-zone], # All points ids that are not the zone
        in_points$id # Point ids that are in range
      )

      abs_info_same_zone <- sum(abs(info[zone, zone]))
      abs_info_other_zone <- sum(abs(info[zone,other_zone]))

      return(-abs_info_same_zone)
    }

    # wrapper for the cluster eval function to restrict to a single argument
    cluster_eval <- function(zone) {
      # handle the edge case that we have 0 * Inf
      profit <- alpha*(avg_dist_profit(zone))
      info <- (1-alpha)*(relevancy(zone))
      if (is.na(profit) & alpha == 0) {
        return(info)
      }
      # else return the weigthed sum
      alpha*(avg_dist_profit(zone)) + (1-alpha)*(relevancy(zone))
    }

    # Operators for the local search (for now there is only insertion)
    # take a point from one cluster and add it to another
    insertion <- function(zones, bks_obj) {

      # Update zones for generating the candidates
      inst_temp <- in_points |>
        dplyr::select(id) |>
        dplyr::left_join(
          tibble::tibble(zone = 1:k, id = zones) |>
            tidyr::unnest(cols = id) |>
            dplyr::filter(id != 1),
          by = c("id" = "id")
        )

      # A candidate should be understood as moving the respective id to the respective zone
      candidates <- inst$edges |>
        dplyr::select(ind1, ind2) |>
        dplyr::filter(
          ind1 != 1, ind2 != 1
        ) |>
        dplyr::inner_join(inst_temp, by = c("ind1" = "id")) |>
        dplyr::inner_join(inst_temp, by = c("ind2" = "id")) |>
        dplyr::filter(zone.x != zone.y) |>
        tidyr::pivot_longer(cols = c(zone.x, zone.y), values_to = "zone_id") |>
        dplyr::mutate(id1 = ifelse(name == "zone.y", ind1, ind2),
                      id2 = ifelse(name == "zone.x", ind1, ind2)) |>
        dplyr::select(id = id1, zone_id) |>
        dplyr::distinct()

      if (nrow(candidates) == 0) {
        warning("There are no candidates, maybe all points are in one zone")
        return(zones)
      }

      # Check if a zone is connected
      connected <- function(zone) {
        # zone <- zones[[1]]
        sub_g <- igraph::induced_subgraph(g, vids = zone)
        igraph::is_connected(sub_g, mode = "weak") # check for undirected path between pairs of vertices
      }

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

        # check if zones are connected
        if (!all(do.call(c, lapply(zones, connected)))) {
          return(Inf)
        }

        return(do.call(sum, lapply(zones, cluster_eval)))
      }

      best_insert <- NA
      best_insert_obj <- bks_obj
      # message("\n")
      for (i in 1:nrow(candidates)) {
        # message("candidates left:", nrow(candidates) - i, "\r")
        id <- candidates[i,] |> dplyr::pull(id)
        zone_id <- candidates[i,] |> dplyr::pull(zone_id)

        candidate_obj <- insert_eval(id, zone_id)
        if (candidate_obj < best_insert_obj) {
          best_insert_obj <- candidate_obj
          best_insert <- candidates[i,]
          break # continue only until a better candidate is found
        }
      }

      if (best_insert_obj == bks_obj) return(zones) # if no better candidate was found return the original zones

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

      return(zones)
    }

    # local search part
    message("Starting the local search")
    iter_max = 100

    initial_obj <- do.call(sum, lapply(zones, cluster_eval))
    message("Inital objective is: ", round(initial_obj, 5))

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
      new_zones <- insertion(zones, bks_obj)
      new_obj <- do.call(sum, lapply(new_zones, cluster_eval))

      if (new_obj < bks_obj) {
        zones <- new_zones
        bks_obj <- new_obj

        # saving data for the convergence plot
        ls_obj[iter + 1] <- new_obj

        # saving data for the zone animation
        zones_list[[iter + 1]] <- zones

        message("iteration: ", iter, "   objective: ", round(bks_obj, 5))
      } else {
        ls_obj[iter + 1] <- ls_obj[iter]
        zones_list[[iter + 1]] <- zones_list[[iter]]
        # message("local search conclude with bks: ", round(bks_obj, 5))
        break
      }

      if (iter == iter_max) {
        message("reached maximum number of iterations")
        break
      }
    }

    # Adjust iter_max if local search concluded
    iter_max <- iter

    # Final objective for profit and variance
    cluster_eval2 <- function(zone) {
      dst_temp <- dst[zone, zone]
      avg_dist <- mean(dst_temp[lower.tri(dst_temp, diag = F)])
      total_profit <- sum(inst$points$score[zone])
      total_variance <- sum(inst$points$score_variance[zone], na.rm = T)
      p <- .05
      q <- qnorm(p, mean = total_profit, sd = sqrt(total_variance))

      tibble::tibble("profit" = total_profit, "q" = q, "avg_dist" = avg_dist)
    }

    obj <- do.call(dplyr::bind_rows, lapply(zones, cluster_eval2)) |>
      dplyr::summarise("avg_dist / total_profit" = sum(avg_dist/profit),
                       "avg_dist / q" = sum(avg_dist/q))

    return(
      list(
        "inst_points" = inst$points |>
          dplyr::select(id, point_type, x, y, score, score_variance) |>
          dplyr::left_join(
            tibble::tibble(zone = 1:k, id = zones) |>
              tidyr::unnest(cols = id) |>
              dplyr::filter(id != 1),
            by = c("id" = "id")
          ) |>
          dplyr::mutate(zone = ifelse(id == 1, 0, zone)),
        "zones" = zones,
        "obj" = obj,
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

  same_zone_edges <- inst$edges |>
    dplyr::left_join(
      inst$points |> dplyr::select(id, zone),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      inst$points |> dplyr::select(id, zone),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  structure(
    list(
      "instance" = inst,
      "k" = k,
      "info" = info,
      "cluster_method" = cluster_method,
      "cl" = cl,
      "g" = g,
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
  # clust <- clustering(inst = test_instances$p7_chao, k = 4, cluster_method = "local_search", variances = generate_variances(test_instances$p7_chao))

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
      data = clust$instance$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::mutate(zone = factor(zone)),
      ggplot2::aes(x, y, color = zone, size = score, alpha = score_variance)
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
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
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
      delsgs_same_zone <- inst$edges |>
        dplyr::left_join(
          plot_points |> dplyr::select(id, zone),
          by = c("ind1" = "id")
        ) |>
        dplyr::left_join(
          plot_points |> dplyr::select(id, zone),
          by = c("ind2" = "id")
        ) |>
        dplyr::filter((zone.x == zone.y) | (is.na(zone.x) | is.na(zone.y))) |>
        dplyr::distinct(x1, y1, x2, y2)
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
      message("iteration", i - 1, "of", clust$plot_data$iter_max)
      print(
        cowplot::plot_grid(
          plot_iter_convergence(iter = i - 1),
          plot_iter_zones(iter = i, delaunay = T)
        )
      )
    }
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
