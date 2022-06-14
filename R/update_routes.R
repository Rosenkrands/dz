#' update routes based on the new information available during mission
#'
#' @param p_inst
#' @param zones
#' @param L
#' @param k
#' @param sr
#' @param info
#'
#' @return
#' @export
#'
update_routes2 <- function(p_inst, zones, L, k, sr, info) {
  # For testing purposes:
  # set.seed(1); inst <- test_instances$p7_chao; L <- 100; k <- 4; variances <- generate_variances(inst); info <- generate_information(inst); p_inst <- prepare_instance(inst, variances, info); rb_clust <- rb_clustering(p_inst, L, k, num_routes = 100, info); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L)

  # helper functions
  sp <- function(id1, id2, graph = g){
    # handle identical ids
    if (id1 == id2) {
      # warning("Trying to calculate the shortest path from one node to itself, returning empty vector")
      return(integer())
    }

    # Find vertices that make up the path
    short_vert <- igraph::shortest_paths(
      graph = graph,
      from = as.character(id1),
      to = as.character(id2),
      output = "vpath"
    )$vpath[[1]] |>
      names() |>
      as.integer()

    # return the path not including the first point
    return(short_vert |> tail(-1))
  }
  plot_progress <- function() {
    temp <- tibble::tibble(id = zones, agent_id = 1:length(zones)) |>
      tidyr::unnest(cols = id)

    inst$points$score <- score

    same_zone_edges <- inst$edges |>
      dplyr::left_join(
        temp |> dplyr::rename(zone = agent_id),
        by = c("ind1" = "id")
      ) |>
      dplyr::left_join(
        temp |> dplyr::rename(zone = agent_id),
        by = c("ind2" = "id")
      ) |>
      dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
      dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
      dplyr::select(-c(zone.x,zone.y))

    route_segments <- tibble::tibble(
      routes = c(route, remaining_route),
      route_type = c(rep("route", length(route)), rep("remaining_route", length(remaining_route)))
    ) |>
      # dplyr::group_by(agent_id) |>
      dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
      dplyr::filter(!is.na(id_start)) |>
      dplyr::select(-routes) |>
      dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                        by = c("id_start" = "id")) |>
      dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                        by = c("id_end" = "id"), suffix = c("","end"))

    ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = same_zone_edges |> dplyr::filter(zone == zone_id),
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = inst$points |>
          dplyr::filter(point_type == "node", id %in% zones[[zone_id]]) |>
          dplyr::left_join(temp, by = c("id")),
        # ggplot2::aes(x, y, color = as.character(agent_id), size = score)
        ggplot2::aes(x, y, size = score), color = "darkgrey"
      ) +
      ggplot2::geom_text(
        data = inst$points |> dplyr::filter(point_type == "node", id %in% zones[[zone_id]]),
        ggplot2::aes(x, y, label = id), nudge_x = .25, nudge_y = 1, size = 2.5
      ) +
      ggplot2::geom_text(
        data = inst$points |> dplyr::filter(point_type == "node", id %in% zones[[zone_id]]),
        ggplot2::aes(x, y, label = round(score, 1)), nudge_x = -.35, nudge_y = 1, size = 2, color = "blue"
      ) +
      ggplot2::geom_segment(
        data = route_segments |> dplyr::filter(route_type == "route"),
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend), color = "black", size = .75
      ) +
      ggplot2::geom_segment(
        data = route_segments |> dplyr::filter(route_type == "remaining_route"),
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend), color = "magenta", linetype = "dashed", size = .75
      ) +
      ggplot2::geom_point(
        data = inst$points |> dplyr::filter(point_type == "terminal"),
        ggplot2::aes(x, y), color = "red", shape = 17, size = 3
      ) +
      ggplot2::ggtitle(paste0("Instance: ", inst$name), subtitle = paste0("id_now: ", id_now, "\n",
                                                                          "route: ", paste(route, collapse = " "), "\n",
                                                                          "remaining_route: ", paste(remaining_route, collapse = " "), "\n",
                                                                          "L_remaining: ", round(L_remaining, 1))) +
      ggplot2::theme_minimal() +
      ggplot2::guides(
        shape = "none",
        fill = "none",
        color = "none",
        alpha = "none",
        size = "none"
      )
  }

  update_route <- function(zone_id) {
    # subgraph for the zone and distances
    sub_g <- igraph::induced_subgraph(p_inst$g, vids = zones[[zone_id]])
    sub_dst <- igraph::distances(sub_g)

    original_route <- sr$improved_routes[[zone_id]]
    L_remaining <- sr$L_remaining[[zone_id]]

    route <- original_route[1:2]
    remaining_route <- original_route[3:length(original_route)]

    # Make copies of variables to alter during route generation
    score <- p_inst$points$score
    expected_score <- p_inst$points$expected_score
    realized_score <- p_inst$points$realized_score
    unexpected <- p_inst$points$unexpected
    candidate_outside <- 0
    # plot_progress()

    while (T) {
      id_now <- tail(route, 1)#; cat("id_now is", id_now, "\n")

      # update score
      score[id_now] <- 0; realized_score[id_now] <- 0

      # update expected score
      if (unexpected[id_now]) { #cat("unexpected observation at id_now, updating expected scores\n")
        related_nodes <- which(info[id_now,] != 0) # find the nodes that are related
        for (id in related_nodes) { # update score for related nodes
          if (!id %in% route){ # Only update the scores for unvisited points
            score[id] <- score[id] + info[id_now,id]
          }
        }
        unexpected[id_now] <- F
      }

      find_best_candidate <- function(sub_g, sub_dst) {
        # calculate SDR for neighbors
        all_nghbrs <- igraph::neighborhood(sub_g, order = 1, nodes = as.character(id_now))[[1]] |>
          names() |> as.integer()
        # nghbrs <- all_nghbrs[(all_nghbrs != id_now) & (!all_nghbrs %in% route)] # remove id_now and nodes already visited
        nghbrs <- all_nghbrs[(all_nghbrs != id_now) & ((!all_nghbrs %in% route) | (all_nghbrs %in% remaining_route))] # remove id_now and nodes already visited

        # which neigbors can feasibly replace remaining_route[1]
        L_removed <- sub_dst[as.character(id_now), as.character(remaining_route[1])] +
          sub_dst[as.character(remaining_route[1]), as.character(remaining_route[2])]
        L_added <- sub_dst[as.character(id_now), as.character(nghbrs)] +
          sub_dst[as.character(nghbrs), as.character(remaining_route[2])]
        delta <- L_added - L_removed
        L_added <- L_added[delta < L_remaining] # L_added now only contain feasible nodes

        if (length(nghbrs) == 1) {names(L_added) <- nghbrs} # Add names to L_added if length is one

        feasible_nghbrs <- nghbrs[nghbrs %in% as.integer(names(L_added))]

        temp_score <- sapply(feasible_nghbrs, function(nghbr){
          temp_path <- sp(as.character(nghbr), remaining_route[2], g = sub_g)
          ids <- c(nghbr, temp_path[-length(temp_path)]) # remove remaining_route[2]
          sum(score[ids])
        })

        sdr <- temp_score / L_added
        # sdr[!is.finite(sdr)] <- 0

        best_candidate <- as.integer(names(which.max(sdr)))
        return(best_candidate)
      }

      if (!find_best_candidate(p_inst$g, p_inst$dst) %in% zones[[zone_id]]) {
        candidate_outside <- candidate_outside + 1
      }

      best_candidate <- find_best_candidate(sub_g, sub_dst)

      if (best_candidate %in% remaining_route) {best_candidate <- remaining_route[1]}

      # if (best_candidate == remaining_route[1]) cat("No better candidate found\n")

      route <- append(route, best_candidate)
      remaining_route <- c(
        sp(best_candidate, remaining_route[2], g = sub_g),
        remaining_route[-(1:2)] # remove substituted node and remaining_route[2] as it is included in sp output
      )

      # update L_remaining
      L_remaining <- L - sum(sub_dst[stats::embed(as.character(c(route, remaining_route)), 2)[, 2:1]])

      # plot_progress()

      if ((remaining_route[1] == 1) & (length(remaining_route) == 1)) {
        id_now <- tail(route, 1)#; cat("id_now is", id_now, "\n")
        score[id_now] <- 0; realized_score[id_now] <- 0

        route <- append(route, 1)
        remaining_route <- integer()
        break
      }
    }

    # plot_progress()

    list(
      "route" = route,
      "total_score" = sum(p_inst$points$score[unique(route)]),
      "total_realized_score" = sum(p_inst$points$realized_score[unique(route)]),
      "L_remaining" = L_remaining,
      "candidate_outside" = candidate_outside
    )
  }

  routes <- lapply(seq_along(zones), update_route)

  structure(
    list(
      "routes" = lapply(routes, function(x) x$route),
      "total_score" = lapply(routes, function(x) x$total_score),
      "total_realized_score" = lapply(routes, function(x) x$total_realized_score),
      "L_remaining" = lapply(routes, function(x) x$L_remaining),
      "candidate_outside" = lapply(routes, function(x) x$candidate_outside),
      "zones" = zones,
      "L" = L
    ),
    class = "updated_routes2"
  )
}

#' Plot method for the updated routes
#'
#' @param ur
#' @param inst
#'
#' @return
#' @export
#'
plot.updated_routes2 <- function(ur, inst) {
  # For testing purposes:
  # set.seed(1); inst <- test_instances$p7_chao; L <- 340/2; k <- 2; variances <- generate_variances(inst); info <- generate_information(inst); p_inst <- prepare_instance(inst, variances, info); rb_clust <- rb_clustering(p_inst, L, k, num_routes = 100, info); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L); ur <- update_routes2(p_inst, zones, L, k, sr, info)

  temp <- tibble::tibble(id = ur$zones, agent_id = 1:length(ur$zones)) |>
    tidyr::unnest(cols = id)

  same_zone_edges <- inst$edges |>
    dplyr::left_join(
      temp |> dplyr::rename(zone = agent_id),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      temp |> dplyr::rename(zone = agent_id),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  route_segments <- tibble::tibble(routes = ur$routes, agent_id = 1:length(ur$routes)) |>
    tidyr::unnest(routes) |>
    dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "node") |>
        dplyr::left_join(temp, by = c("id")),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score)
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      # color = "none",
      alpha = "none",
      size = "none"
    ) +
    ggplot2::labs(
      color = "Zone"
    ) +
    ggplot2::coord_fixed()
}
