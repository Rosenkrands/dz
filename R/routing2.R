#' Find the starting routes based on provided zones
#'
#' @param inst
#' @param zones
#' @param L
#'
#' @return
#' @export
#'
starting_routes <- function(inst, zones, L) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20);p_inst <- prepare_instance(inst, variances, info); rb_clust <- rb_clustering(p_inst, L, k, num_routes = 100, info); zones <- rb_clust$zones

  # Join zones on instance
  all_points <- inst$points |>
    dplyr::left_join(
      tibble::tibble(id = zones, zone = 1:length(zones)) |>
        tidyr::unnest(cols = id) |>
        dplyr::filter(id != 1),
      by = c("id")
    )

  all_points$zone[1] <- 0

  # clustering_result <- clustering(inst = test_instances$p7_chao, k = 3, L = 100, eps = 0, cluster_method = "local_search", variances = generate_variances(inst), alpha = 0, info <- generate_information(inst, r = 100))

  same_zone_edges <- inst$edges |>
    dplyr::left_join(
      all_points |> dplyr::select(id, zone),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      all_points |> dplyr::select(id, zone),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  # Function for calculating the distance of the shortest (DL) path between 2 points.
  dist <- function(id1, id2, g){
    # Find vertices that make up the path
    if (id1 == id2) return(0)
    short_vert <- as.vector(igraph::shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
    # Calculate total distance between them
    route_length <- 0
    dist_matrix <- igraph::distances(g)
    for (node in 1:(length(short_vert)-1)){
      temp <- dist_matrix[short_vert[node], short_vert[node+1]]
      route_length <- route_length + temp
    }
    return(route_length)
  }

  # Dist function that returns only the points in the path
  dist2 <- function(id1, id2, g){
    # Find vertices that make up the path
    if (id1 == id2) return(0)
    short_vert <- as.vector(igraph::shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
    return(short_vert)
  }

  ### Function for route length
  route_length <- function(route, g) {
    distance_temp <- vector(length = length(route)-1)
    for (placement in (1):(length(route)-1)) {
      # print(placement)
      distance_temp[placement] <- dist(route[placement], route[placement + 1], g = g)
    }
    return(sum(distance_temp))
  }

  ### Function for route score
  # Use placement of id_next instead of the node id
  route_score <- function(route, id_next_placement) {
    # route <- unique(route)
    score_temp_realized <- vector(length = id_next_placement)
    score_temp_expected <- vector(length = (length(route) - (id_next_placement)))
    for (placement in (1):(length(score_temp_realized)-1)) {
      score_temp_realized[placement] <- map$score[placement]
    }
    for (placement in (1):(length(score_temp_expected)-1)) {
      score_temp_expected[placement] <- map$score[placement]
    }
    return(sum(score_temp_realized, na.rm = T) + sum(score_temp_expected, na.rm = T))
  }

  # solve routing for each zone to get initial route
  solve_routing <- function(obj = 'SDR', L = 100, zone_id = 1){
    # obj = 'SDR'; L = 100; zone_id = 1
    L_remaining <- L
    map = all_points |>
      dplyr::filter((id == 1) | (zone == zone_id))

    delsgs <- same_zone_edges |>
      dplyr::filter(zone == zone_id) |>
      tibble::as_tibble()

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # adapt to correct ids (by converting to local ids)
    lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = everything())
    delsgs <- delsgs |>
      dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
      dplyr::select(-ind1, ind1 = local_id) |>
      dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
      dplyr::select(-ind2, ind2 = local_id)

    # create igraph object with local ids
    g <- igraph::graph.data.frame(delsgs |>  dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map |> dplyr::select(local_id, score))

    candidates <- map$local_id
    route = integer()
    route <- append(route, 1)
    last_in_current <- route[length(route)]
    route <- append(route, 1)
    s_total <- 0
    while (L_remaining > 0) {
      # print(lookup$id[route])
      if (obj == 'SDR'){
        d <- vector(length = length(map$id))
        s <- vector(length = length(map$id))
        s_path <- vector()
        SDR <- vector(length = length(map$id))
        for (i in 1:length(candidates)) {
          route_temp <- route
          route_temp <- append(route_temp, candidates[i], after = length(route_temp)-1)
          # d[i] <- dist(route[length(route)], candidates[i], g = g) +
          #   dist(candidates[i], route[length(route)-1], g = g) -
          #   as.vector(dist(route[length(route_temp)-2], route_temp[1], g = g))
          #
          d[i] <- dist(route[length(route)-1], candidates[i], g = g) +
            dist(candidates[i], 1, g = g) -
            as.vector(dist(route[length(route_temp)-2], 1, g = g))
          # Make vector of nodes in path to and from candidate, to base score of
          sp1_nodes <- dist2(route[length(route)-1], candidates[i], g = g)
          sp2_nodes <- dist2(candidates[i], 1, g = g)
          s_path <- c(sp1_nodes, sp2_nodes[2:(length(sp2_nodes))])
          s[i] <- sum(map$score[unique(s_path)])
          # s[i] <- map[candidates[i],]$score
          SDR[i] <- s[i]/d[i]
          SDR[1] <- 0
          SDR[is.na(SDR)] <- 0
          SDR[!is.finite(SDR)] <- 0
        }
        New_last <- which.max(SDR)
        sp1_nodes_g <- dist2(route[length(route)-1], New_last, g = g)
        sp2_nodes_g <- dist2(New_last, 1, g = g)
        s_path_g <- c(sp1_nodes_g, sp2_nodes_g[2:(length(sp2_nodes_g))])
        # s_total <- sum(map$score[unique(s_path_g)]) + s_total
        # map$score[unique(s_path_g)] <- 0
        all_short_path <- dist2(route[length(route)-1], New_last, g = g)
        for (node in (all_short_path[2:length(all_short_path)])) {
          s_total <- s_total + map$score[node]
          map$score[node] <- 0
        }
      }
      if (obj == 'random'){
        New_last <- sample(2:101, size = 1)
        all_short_path <- dist2(route[length(route)-1], New_last, g = g)
        s_total <- s_total + map[New_last,]$score
        map[New_last,]$score <- 0
        # print(New_last)
      }
      while ((dist(last_in_current, New_last, g = g) + dist(New_last, 1, g = g) - dist(last_in_current,  1, g = g)) > L_remaining){ # if there is not enough range to visit new last, then check the next one
        SDR[New_last] <- 0
        New_last <- which.max(SDR)
        print(New_last)
        print(SDR[New_last])
        if (SDR[New_last] == 0) {# No SDR candidates can be reached, add route back to base
          all_short_path_return <- dist2(route[(length(route)-1)], 1, g = g)
          route <- append(route, all_short_path_return[2:(length(all_short_path_return)-1)], after = length(route)-1)
          ### Remove duplicate e.g. 1 56 ... 30 1 30 1
          for (i in 1:(length(route)-3)) {
            if (route[i] == route[i+2] & route[i+1] == route[i+3]) {
              route <- route[-i]; route <- route[-i]
            }
          }
          i = 1
          while (i %in% (1:(length(route)-3))) {
            if (is.na(route[i+3]) | is.na(route[i+2])) {
              break
            }
            if (route[i] == route[i+2] & route[i+1] == route[i+3]) {
              route <- route[-i]; route <- route[-i]
              i = i - 2
            }
            i = i + 1
          }
          route_global <- vector(length = length(route))
          for (i in 1:length(route)){
            route_global[i] <- lookup$id[route[i]]
          }
          L_remaining <- L - route_length(route = route, g = g)
          # Function to plot path using information in route object
          output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
          print(route)
          return(output)
        }
      }
      #HERE
      route_temp <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
      print(route)
      # construct route to determine length, including path to the source
      #HERE
      # route_temp <- c(route[-length(route)], dist2(route[length(route) - 1], 1, g = g)[-1])
      # route_global <- vector(length = length(route_temp))
      # for (i in 1:length(route_temp)){
      #   route_global[i] <- lookup$id[route_temp[i]]
      # }
      #HERE
      L_remaining <- L - route_length(route = route_temp, g = g)
      if(L_remaining < 0) {
        route_global <- vector(length = length(route))
        for (i in 1:length(route)){
          route_global[i] <- lookup$id[route[i]]
          output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
        }
        return(output)
      } else {
        route <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
      }
      # while (L_remaining < 0) {
      #   # route[-(length(route)-1)]
      #   route_temp <- c(route[-((length(route)-1):length(route))], dist2(route[length(route) - 1], 1, g = g)[-1])
      #   L_remaining <- L - route_length(route = route_temp, g = g)
      #   SDR <- rep(0, length(SDR))
      #   print(route_temp)
      #   route <- route_temp
      # }

      # print(route)
      # if (L_remaining < 50) {
        # route_global <- vector(length = length(route))
        # for (i in 1:length(route)){
        #   route_global[i] <- lookup$id[route[i]]
        # }
      #   output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
      #   return(output)
      # }
      # print(SDR)
      #HERE
      # if (max(SDR) == 0){
      #   # If last before source is not directly connected to source
      #   if (length((dist2(route[(length(route)-1)], route[length(route)], g = g))) > 2) {
      #     # Connect them if L_remaining allows it, otherwise remove some of the route
      #     if (route_length(route = route, g = g) <= L){
      #       # Add shortest path to source to "route"
      #       sp_last <- dist2(route[(length(route)-1)], route[length(route)], g = g)
      #       route <- append(route, sp_last[2:(length(sp_last)-1)], after = (length(route)-1))
      #       # Make sure we return after this
      #       # SDR <- rep(0, length(SDR))
      #     } else {
      #       while (route_length(route = route, g = g) > L) {
      #         # Remove last in route
      #         route <- c(route[-((length(route)-1):length(route))], dist2(route[length(route) - 1], 1, g = g)[-1])
      #         L_remaining <- L - route_length(route = route_temp, g = g)
      #       }
      #       sp_last <- dist2(route[(length(route)-1)], route[length(route)], g = g)
      #       route <- append(route, sp_last[2:(length(sp_last)-1)], after = (length(route)-1))
      #       # SDR <- rep(0, length(SDR))
      #     }
      #   }
      #   route_global <- vector(length = length(route))
      #   for (i in 1:length(route)){
      #     route_global[i] <- lookup$id[route[i]]
      #   }
      #   ### Remove duplicate e.g. 1 56 ... 30 1 30 1
      #   #HERE
      #   # for (i in (length(route_global)-3)) {
      #   #   if (route_global[i] == route_global[i+2] & route_global[i+1] == route_global[i+3]) {
      #   #     route_global <- route_global[-i]; route_global <- route_global[-i]
      #   #   }
      #   # }
      #   output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
      #   return(output)
      # }
    }
  }

  # Use the result from solve_routing and update by adding until no more range in the same way as solve_routing

  improve_routing <- function(L_remaining, L, route, zone_id){
    # L_remaining = r$L_remaining; route = r$route; zone_id = 1
    # route <- lookup$local_id[match(lookup$local_id, route)]

    L_remaining <- L_remaining/2

    map = all_points |>
      dplyr::filter((id == 1) | (zone == zone_id))

    delsgs <- same_zone_edges |>
      dplyr::filter(zone == zone_id) |>
      tibble::as_tibble()

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # adapt to correct ids (by converting to local ids)
    lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = everything())
    delsgs <- delsgs |>
      dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
      dplyr::select(-ind1, ind1 = local_id) |>
      dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
      dplyr::select(-ind2, ind2 = local_id)

    # create igraph object with local ids
    g <- igraph::graph.data.frame(delsgs |>  dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map |> dplyr::select(local_id, score))

    route <- sapply(route, function(x) lookup$local_id[lookup$id == x])

    # Fix instant repeat of nodes in route
    j = 1
    while (j %in% (1:(length(route)-1))) {
      if (is.na(route[j+1])) {
        break
      }
      if (route[j] == route[j+1]) {
        route <- route[-j]
        j = j - 1
      }
      j = j + 1
    }

    map$score[route] <- 0

    candidates <- map$local_id[!(map$local_id) %in% route]
    # last_in_current <- route[length(route)]
    s_total <- 0

    d <- list()
    s <- list()
    SDR <- list()
    rl <- length(route)-1
    while (L_remaining > 0) {
      for (n in 1:rl) {
        d[[n]] <- vector(length = length(candidates))
        s[[n]] <- vector(length = length(candidates))
        SDR[[n]] <- vector(length = length(candidates))
        for (i in (1:(length(candidates)))) {
          route_temp <- route
          route_temp <- append(route_temp, candidates[i], after = n)
          d[[n]][i] <- dist(route_temp[n], candidates[i], g = g) +
            dist(candidates[i], route_temp[n+2], g = g) -
            dist(route_temp[n], route_temp[n+2], g = g)
          # Uses SDR for all nodes is the shortest paths
          nodes_visited <- unique(c(dist2(route[n], candidates[i], g = g), dist2(candidates[i], route[n+1], g = g)))
          s[[n]][i] <- sum(map$score[nodes_visited])
          SDR[[n]][i] <- (s[[n]][i])/(d[[n]][i])
          SDR[[n]][i][!is.finite(SDR[[n]][i])] <- 0
          if (d[[n]][i] > L_remaining){
            SDR[[n]][i] <- 0
          }
        }
      }
      New_node <- vector(length = rl)
      value <- vector(length = rl)
      # New_node[n] <- which.max(SDR[[n]])
      for (nr in 1:rl) {
        New_node[nr] <- which.max(SDR[[nr]])
        value[nr] <- max(SDR[[nr]])
      }
      if (max(value) == 0) {
        route_global <- vector(length = length(route))
        for (i in 1:length(route)){
          route_global[i] <- lookup$id[route[i]]
        }
        L_remaining <- L - route_length(route = route, g = g)
        output <- list("route" = route_global, "L_remaining" = L_remaining, "delsgs" = delsgs, "lookup" = lookup)
        return(output)
      }
      # Værdien i New_node er candidate id_local med højest SDR og indgangen er hvor i ruten det tilføjes efter
      New_node_placement <- which.max(value)
      ### Insert New_node in the right place
      # Includes shortest path to and from new node, not just the highest SDR node itself
      short_path_to <- dist2(route[New_node_placement], candidates[New_node[New_node_placement]], g = g)
      short_path_back <- dist2(candidates[New_node[New_node_placement]], route[(New_node_placement+1)], g = g)
      route <- append(route, short_path_to[2:length(short_path_to)], after = New_node_placement)
      route <- append(route, short_path_back[2:(length(short_path_back)-1)], after = (New_node_placement + length(short_path_to) - 1))
      # route <- append(route, New_node[New_node_placement], after = New_node_placement)
      # Update L_remaining
      L_remaining <- L - route_length(route, g = g)
      # Update score
      # map$score[New_node[New_node_placement]] <- 0
      map$score[unique(c(short_path_to, short_path_back))] <- 0
      ### Remove duplicate e.g. 1 56 ... 30 1 30 1
      # for (i in 1:(length(route)-4)) {
      #   if ((route[i] == route[i+2]) && (route[i+1] == route[i+3])) {
      #     route <- route[-i]; route <- route[-i]
      #   }
      # }
      i = 1
      while (i %in% (1:(length(route)-3))) {
        if (is.na(route[i+3])) {
          break
        }
        if ((route[i] == route[i+2]) && (route[i+1] == route[i+3])) {
          route <- route[-i]; route <- route[-i]
          i = i - 2
        }
        i = i + 1
      }
    }
    route_global <- vector(length = length(route))
    for (i in 1:length(route)){
      route_global[i] <- lookup$id[route[i]]
    }
    L_remaining <- L - route_length(route = route, g = g)
    output <- list("route" = route_global, "L_remaining" = L_remaining, "delsgs" = delsgs, "lookup" = lookup)
    return(output)
  }

  # Testing
  # r <- solve_routing(zone_id =  1)
  # imr <- improve_routing(L_remaining = r$L_remaining, route = r$route, L = 100, zone_id = 1)

  # we want to create a route for each zone
  routing_results <- tibble::tibble(agent_id = 1:length(zones))

  # calculate the routes
  message("Finding the initial routes")
  initial_routes <- pbapply::pblapply(
    routing_results$agent_id,
    function(zone_id) {solve_routing(obj = "SDR", L = L, zone_id = zone_id)}
  )

  message("Trying to improve initial routes")
  improved_routes <- pbapply::pblapply(
    routing_results$agent_id,
    function(zone_id) {improve_routing(
      L_remaining = initial_routes[[zone_id]]$L_remaining,
      route = initial_routes[[zone_id]]$route,
      # L = L - initial_routes[[zone_id]]$L_remaining/2,
      L = L,
      zone_id = zone_id
    )}
  )

  # return(initial_routes)

  structure(
    list(
      "initial_routes" = lapply(initial_routes, function(x) x$route),
      "improved_routes" = lapply(improved_routes, function(x) x$route),
      "L_remaining" = lapply(improved_routes, function(x) x$L_remaining),
      "total_score" = lapply(improved_routes, function(x) sum(inst$points$score[unique(x$route)])),
      "zones" = zones,
      "L" = L
    ),
    class = "starting_routes"
  )
}

#' Plot method for the starting routes
#'
#' @param sr
#' @param inst
#'
#' @return
#' @export
#'
plot.starting_routes <- function(sr, inst) {
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); p_inst <- prepare_instance(inst, variances, info) rb_clust <- rb_clustering(p_inst, L, k, num_routes = 100, info); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L)

  temp <- tibble::tibble(id = sr$zones, agent_id = 1:length(sr$zones)) |>
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

  route_segments <- tibble::tibble(routes = sr$improved_routes, agent_id = 1:length(sr$improved_routes)) |>
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
        dplyr::filter(point_type == "intermediate") |>
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
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
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

# inst = test_instances$p7_chao; L = 100; k = 3
# info <- generate_information(inst = inst, r = 20)
# generated_variances <- generate_variances(inst = inst)
# p_inst <- prepare_instance(inst, variances = generated_variances, info = info)
# clust_obj <- rb_clustering(p_inst = p_inst, num_route = 100, info = info, k = 3, L = 100)
#
# sr <- starting_routes(inst = inst, zones = clust_obj$zones, L = 100)


#' Update the starting routes based on realized scores
#'
#' @param sr
#' @param L
#' @param variances
#' @param info
#'
#' @return
#' @export
#'
#' @examples

update_routes <- function(sr, L, variances, info) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); p_inst <- prepare_instance(inst, variances = generated_variances, info = info); rb_clust <- rb_clustering(p_inst = p_inst, num_route = 100, info = info, k = 3, L = 100); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L)

  zones <- sr$zones
  # zones <- sr$lookup$id

  # Join zones on instance
  map <- inst$points |>
    dplyr::left_join(
      tibble::tibble(id = zones, zone = 1:length(zones)) |>
        tidyr::unnest(cols = id) |>
        dplyr::filter(id != 1),
      by = c("id")
    ) |>
    dplyr::left_join(variances, by = c("id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(realized_score = ifelse(is.na(score_variance), NA, rnorm(1, mean = score, sd = sqrt(score_variance)))) |>
    dplyr::mutate(unexpected = purrr::rbernoulli(1, p = p_unexpected)) |>
    dplyr::ungroup()

  map$zone[1] <- 0
  map$realized_score[1] <- 0
  map$unexpected[1] <- FALSE

  same_zone_edges <- inst$edges |>
    dplyr::left_join(
      map |> dplyr::select(id, zone),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      map |> dplyr::select(id, zone),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  dist <- function(id1, id2, dst){
    # Find vertices that make up the path
    if (id1 == id2) return(0)
    # short_vert <- as.vector(igraph::shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
    # Calculate total distance between them
    route_length <- dst[id1, id2]
    # dist_matrix <- igraph::distances(g)
    # for (node in 1:(length(short_vert)-1)){
    #   temp <-
    #   route_length <- route_length + temp
    # }
    return(route_length)
  }

  # Dist function that returns only the points in the path
  dist2 <- function(id1, id2, g){
    # Find vertices that make up the path
    if (id1 == id2) return(0)
    short_vert <- as.vector(igraph::shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
    return(short_vert)
  }

  # update routes for each zone (multiple times)
  update_routing <- function(initial_route, zone_id, L, L_remaining) {
    # initial_route = sr$initial_routes[[3]]; zone_id = 3; L = 100; L_remaining = sr$L_remaining[[3]]

    delsgs <- same_zone_edges |>
      dplyr::filter(zone == zone_id) |>
      tibble::as_tibble()

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # Get vector of node ids in zone
    zone <- c(1,map |> dplyr::filter(zone == zone_id) |> dplyr::pull(id))
    # g <- igraph::induced_subgraph(test_instances$p7_chao$g, vids = zone)
    g <- igraph::graph.data.frame(delsgs |> dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map  |>  dplyr::select(id, score))
    dst <- igraph::distances(g)
    # L <- 150
    # initial_route <- solve_routing(L = L)
    # remaining_route <- initial_route$route
    remaining_route <- initial_route
    remaining_nodes <- c(remaining_route[3:length(remaining_route)])
    route <- remaining_route[1:2]
    # print(plot_progress())
    nodes_in_zone <- zone
    # L_remaining <- initial_route$L_remaining

    ### Function for route length
    route_length <- function(route) {
      distance_temp <- vector(length = length(route)-1)
      for (placement in (1):(length(route)-1)) {
        distance_temp[placement] <- dist(route[placement], route[placement + 1], dst = dst)
      }
      return(sum(distance_temp))
    }

    ### Function for route score
    # Use placement of id_next instead of the node id
    route_score <- function(route, id_next_placement) {
      # route <- unique(route)
      score_temp_realized <- vector(length = id_next_placement)
      score_temp_expected <- vector(length = (length(route) - (id_next_placement)))
      for (placement in (1):(length(score_temp_realized)-1)) {
        score_temp_realized[placement] <- map$realized_score[placement]
      }
      for (placement in (1):(length(score_temp_expected)-1)) {
        score_temp_expected[placement] <- map$score[placement]
      }
      return(sum(score_temp_realized, na.rm = T) + sum(score_temp_expected, na.rm = T))
    }

    while(length(remaining_nodes) != 0){
      cat("The route so far:", "\n")
      print(route)
      cat("Route based on original that can still be followed:", "\n" )
      print(remaining_route)
      # Keep track of changes
      last_remaining_route <- remaining_route
      # Node the UAV flew from
      # if (remaining_route[1] == 86){break}
      id_now <- remaining_route[1]
      cat("now, next", "\n")
      print(id_now)
      # The node currently occupied by the UAV
      id_next <- remaining_route[2]
      print(id_next)
      # Since it has been visited the score is updated
      map$realized_score[id_now] <- 0
      map$realized_score[id_next] <- 0
      candidates <- nodes_in_zone
      # Update realized score depending on other visited nodes
      for (node_i in route) {
        # If a node has an unexpectedly high/low realized score the related nodes are updated
        if (map$unexpected[node_i]) {
          other_nodes <- map$id[-node_i]
          for (node_j in other_nodes) {
            # TODO: Update with real relation factor between scores
            corr = 1
            map$realized_score[node_j] <- (map$realized_score)[node_j] + corr * (map$realized_score)[node_i]
          }
          map$unexpected[node_i] <- FALSE
        }
      }
      # Evaluate how good the next planned node to be visited is when using realized score
      if(is.na(remaining_nodes[2])) {remaining_nodes[2] <- 1}
      d_planned_realized <- dist(id_next, remaining_nodes[1],  dst = dst) +
        dist(remaining_nodes[1], remaining_nodes[2], dst = dst)
      s_planned_realized <- map$realized_score[(remaining_nodes[1])] + map$realized_score[(remaining_nodes[2])]
      SDR_planned_realized <- s_planned_realized/d_planned_realized
      # How this compares to the alternative nodes that can be visited
      sp_cand_1 <- list(length = length(map$id))
      d_cand_1 <- vector(length = length(map$id))
      sp_cand_2 <- list(length = length(map$id))
      d_cand_2 <- vector(length = length(map$id))
      d_cand_tot <- vector(length = length(map$id))
      s_cand_tot <- vector(length = length(map$id))
      SDR_cand <- vector(length = length(map$id))
      # Calculate SDR for each new route segment resulting from using a candidate in the route
      for (candidate in candidates[candidates != id_next]){
        # print(candidate)
        # From current to candidate
        d_cand_1[candidate] <- dist(id_next, candidate, dst = dst)
        sp_cand_1[[candidate]] <- dist2(id_next, candidate, g = g)
        # Add score for points visited
        for (sp_node in (sp_cand_1[[candidate]])) {
          s_cand_tot[candidate] <- s_cand_tot[candidate] + map$realized_score[sp_node]
        }
        # From candidate to remainder of original route
        d_cand_2[candidate] <- dist(candidate, remaining_nodes[2], dst = dst)
        sp_cand_2[[candidate]] <- dist2(candidate, remaining_nodes[2], g = g)
        for (sp_node in sp_cand_2[[candidate]]) {
          # handle the case of candidate being equal to remaining_nodes[2]
          # realized_score <- map$realized_score[unique(c(sp_cand_1[[candidate]], sp_cand_2[[candidate]]))]
          realized_score <- map$realized_score[sp_node]
          if (length(realized_score) == 0) {realized_score <- map$realized_score[candidate]}
          s_cand_tot[candidate] <- s_cand_tot[candidate] + realized_score
        }
        # Summarized
        d_cand_tot[candidate] <- d_cand_1[candidate] + d_cand_2[candidate]
        SDR_cand[candidate] <- (s_cand_tot[candidate])/(d_cand_tot[candidate])
      }
      New_point <- which.max(SDR_cand)
      cat("New_point:", "\n")
      print(New_point)
      # Add and remove these from the route according to (shortest paths) SDR
      # We remove two and add at least two, so we need to track how many more we add to route
      longer_than_original <- 0
      # Check length constraint
      if (route[length(route)] != 1) {
        sp_check <- dist2(route[(length(route))], 1, g = g)
        route_check <- append(route, sp_check[2:(length(sp_check))], after = (length(route)))

      } else {
        route_check <- route
      }
      cat("Route is now:", "\n")
      print(route_check)
      L_remaining <- L - route_length(route = route_check)
      cat("Remaining length:", "\n")
      print(L_remaining)
      L_required <- dist(id_next, New_point, dst = dst) + dist(New_point, remaining_nodes[2], dst = dst) + dist(remaining_nodes[2], 1, dst = dst)
      while ((L_remaining < L_required) && (length(remaining_nodes != 0))) {
        SDR_cand[New_point] <- 0
        New_point <- which.max(SDR_cand)
        cat("New_point updated:", "\n")
        print(New_point)
        L_required <- dist(id_next, New_point, dst = dst) + dist(New_point, remaining_nodes[2], dst = dst) + dist(remaining_nodes[2], 1, dst = dst)
        if (max(SDR_cand) == 0) {
          print("max SDR cand is 0")
          if (remaining_nodes[2] == 1) {
            route_final <- append(route, 1)
          } else {
            sp_rm2_home <- dist2(remaining_nodes[2], 1, g = g)
            route_final <- append(route, sp_rm2_home)
          }
          print(route_final)
          print(remaining_nodes[2])
          if (L > route_length(route_final)) {
            print("Can reach rm2 before returning")
            print(route_final)
            route <- route_final
          } else {
            route <- append(route, dist2(route[length(route)], 1, g = g)[-1])
          }
          remaining_nodes <- c()
          break
        }
      }
      # if (remaining_route[1] == remaining_route[3]) {remaining_route <- remaining_route[3:length(remaining_route)]}

      if ((max(SDR_cand) > SDR_planned_realized)  & !(New_point %in% remaining_route) & (L_remaining > L_required) ){
        # Remove the node that would originally be visited after id_next
        map$realized_score[New_point] <- 0
        cat("Remaining nodes:", "\n")
        print(remaining_nodes)
        remaining_route <- remaining_route[2:(length(remaining_route))]
        # Add new
        sp <- c(dist2(id_next, New_point, g = g)[2:(length(dist2(id_next, New_point, g = g)))],
                (dist2(New_point, remaining_nodes[2], g = g)[2:((length(dist2(New_point, remaining_nodes[2], g = g))))]))
        cat("Added sp:", "\n")
        map$realized_score[sp] <- 0
        print(sp)
        # Remove the extra start of end of original
        # remaining_route <- remaining_route[remaining_route != (remaining_nodes[2])]
        remaining_route <- c(remaining_route[1], remaining_route[3:(length(remaining_route))])
        remaining_route <- append(remaining_route, sp, after = 2)
        longer_than_original <- longer_than_original + (length(remaining_route) - length(last_remaining_route))
        if (is.na(remaining_route[3])) {route <- append(route, c(remaining_route[2], 1)); break}
        route <- append(route, remaining_route[3:(length(sp)+2)])
        remaining_route <- remaining_route[-(1:(length(sp)))]
        map$realized_score[New_point] <-  0
        cat("Added a node not in original route", "\n")
        print(New_point)
      } else {
        if (is.na(remaining_route[3])) {
          cat("Remaining route:", "\n")
          print(remaining_route)
          # route <- route[1:(length(route)-1)]
          sp_home <- dist2(id_next, 1, g = g)
          route <- append(route, sp_home[2:length(sp_home)])
          break
        }

        if (route[length(route)] != 1) {
          # Go where we would anyway
          route <- append(route, remaining_route[3])
          # Update remaining_route by removing the ones already visited (excluding id_now and id_next for the next iteration)
          remaining_route <- remaining_route[2:(length(remaining_route))]
        }
      }
      # Update remaining_nodes
      remaining_nodes <- remaining_nodes[remaining_nodes != (remaining_nodes[1])]
      if (length(remaining_nodes) == 0) {break}
      if ((route[length(route)]) == (remaining_nodes[1])) {remaining_nodes <- remaining_nodes[remaining_nodes != remaining_nodes[1]]}
    }
    if(route[length(route)] != 1){
      route <- route[1:(length(route)-1)]
      if(!(New_point %in% remaining_route) & (New_point %in% route)){
        route <- append(route, New_point)
      }
      sp_home <- dist2(id_next, 1, g = g)
      route <- append(route, sp_home[2:length(sp_home)])
    }
    # Return L and Score with the routes
    # print(plot_progress())
    output <- list("route" = route, "s_total" = route_score(route, id_next_placement = length(route)), "L_remaining" = L - route_length(route))
    return(output)
  }

  # sr <- starting_routes(inst = inst, zones = rb_clust$zones, L = 100)
  # update_routing(initial_route = sr$improved_routes[[1]], zone_id = 1, L = 100, L_remaining = sr$L_remaining[[1]])


  # we want to create a route for each zone
  routing_results <- tibble::tibble(agent_id = 1:length(zones))

  # calculate the routes
  updated_routes <- lapply(
    routing_results$agent_id,
    function(zone_id) {update_routing(
      initial_route = sr$initial_routes[[zone_id]],
      zone_id = zone_id,
      L,
      L_remaining = sr$L_remaining[[zone_id]]
    )}
  )

  # report results
  structure(
    list(
      "updated_routes" = lapply(updated_routes, function(x) x$route),
      "L_remaining" = lapply(updated_routes, function(x) x$L_remaining),
      "s_total" = lapply(updated_routes, function(x) x$s_total),
      "zones" = zones,
      "L" = L
    ),
    class = "updated_routes"
  )
}

# ur <- update_routes(sr = sr, L = sr$L, variances = generated_variances, info = info)
# g = test_instances$p7_chao$g

# inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); rb_clust <- rb_clustering(inst, L, k, num_routes = 100, variances, info); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L)
#
# update_routes(sr, L, variances, info)

#' Plot method for the updated routes
#'
#' @param ur
#' @param inst
#'
#' @return
#' @export
#'
#' @examples
plot.updated_routes <- function(ur, inst) {
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); rb_clust <- rb_clustering(inst, L, k, num_routes = 100, variances, info); zones <- rb_clust$zones; sr <- starting_routes(inst, zones, L); ur <- update_routes(sr, L, variances, info)

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

  route_segments <- tibble::tibble(routes = ur$updated_routes, agent_id = 1:length(ur$updated_routes)) |>
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
        dplyr::filter(point_type == "intermediate") |>
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
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
    )
}
