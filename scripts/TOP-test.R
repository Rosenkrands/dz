library(dz)

inst <- test_instances$p7_chao
L <- 125

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

    # Join zones on instance
    all_points <- inst$points |>
      dplyr::left_join(
        tibble::tibble(id = zones, zone = 1:length(zones)) |>
          tidyr::unnest(cols = id) |>
          dplyr::filter(id != 1),
        by = c("id")
      )

    all_points$zone[1] <- 0

    map = all_points |>
      dplyr::filter((id == 1) | (zone == zone_id))

    lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)

    # Compute edges in delaunay triangulation
    inst$edges <- (deldir::deldir(map$x, map$y))$delsgs

    # construct the igraph object
    inst$edges$dist <- sqrt((inst$edges$x1 - inst$edges$x2)^2 + (inst$edges$y1 - inst$edges$y2)^2) # We assume euclidean distance

    # TESTING
    # edge_ids <- c(inst$edges[,1], inst$edges[,2])
    # node_ids <- lookup$local_id
    #
    # unique(edge_ids)[!unique(edge_ids) %in% node_ids]

    inst$g <- igraph::graph.data.frame(
      inst$edges |> dplyr::select(ind1, ind2, weight = dist),
      directed = FALSE
      ,vertices = lookup$local_id
    )

    # calculate distance matrix (based on shortest path)
    inst$dst <- igraph::distances(inst$g, algorithm = "dijkstra")

    delsgs <- inst$edges

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # adapt to correct ids (by converting to local ids)
    # lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = everything())
    # delsgs <- delsgs |>
    #   dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
    #   dplyr::select(-ind1, ind1 = local_id) |>
    #   dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
    #   dplyr::select(-ind2, ind2 = local_id)

    # create igraph object with local ids
    g <- igraph::graph.data.frame(delsgs |> dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map |> dplyr::select(local_id, score)
    )

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
          # print(i)
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
        L_remaining <- L - route_length(route = route, g = g)
        route_global <- vector(length = length(route))
        for (i in 1:length(route)){
          route_global[i] <- lookup$id[route[i]]
          output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
        }
        return(output)
      } else {
        route <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
      }
    }
  }

  # Use the result from solve_routing and update by adding until no more range in the same way as solve_routing

  improve_routing <- function(L_remaining, L, route, zone_id){
    # L_remaining = r$L_remaining; route = r$route; zone_id = 1
    # route <- lookup$local_id[match(lookup$local_id, route)]

    # L_remaining <- L_remaining/2

    ##### OLD
    # map = all_points |>
    #   dplyr::filter((id == 1) | (zone == zone_id))
    #
    # delsgs <- same_zone_edges |>
    #   dplyr::filter(zone == zone_id) |>
    #   tibble::as_tibble()
    #
    # delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)
    #
    # # adapt to correct ids (by converting to local ids)
    # lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    # map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = everything())
    # delsgs <- delsgs |>
    #   dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
    #   dplyr::select(-ind1, ind1 = local_id) |>
    #   dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
    #   dplyr::select(-ind2, ind2 = local_id)
    #
    # # create igraph object with local ids
    # g <- igraph::graph.data.frame(delsgs |>  dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map |> dplyr::select(local_id, score))

    ##### NEW
    # Join zones on instance
    all_points <- inst$points |>
      dplyr::left_join(
        tibble::tibble(id = zones, zone = 1:length(zones)) |>
          tidyr::unnest(cols = id) |>
          dplyr::filter(id != 1),
        by = c("id")
      )

    all_points$zone[1] <- 0

    map = all_points |>
      dplyr::filter((id == 1) | (zone == zone_id))

    lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)

    # Compute edges in delaunay triangulation
    inst$edges <- (deldir::deldir(map$x, map$y))$delsgs

    # construct the igraph object
    inst$edges$dist <- sqrt((inst$edges$x1 - inst$edges$x2)^2 + (inst$edges$y1 - inst$edges$y2)^2) # We assume euclidean distance

    # TESTING
    # edge_ids <- c(inst$edges[,1], inst$edges[,2])
    # node_ids <- lookup$local_id
    #
    # unique(edge_ids)[!unique(edge_ids) %in% node_ids]

    inst$g <- igraph::graph.data.frame(
      inst$edges |> dplyr::select(ind1, ind2, weight = dist),
      directed = FALSE
      ,vertices = lookup$local_id
    )

    # calculate distance matrix (based on shortest path)
    inst$dst <- igraph::distances(inst$g, algorithm = "dijkstra")

    delsgs <- inst$edges

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # adapt to correct ids (by converting to local ids)
    # lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = everything())
    # delsgs <- delsgs |>
    #   dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
    #   dplyr::select(-ind1, ind1 = local_id) |>
    #   dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
    #   dplyr::select(-ind2, ind2 = local_id)

    # create igraph object with local ids
    g <- igraph::graph.data.frame(delsgs |> dplyr::select(ind1, ind2, weight = dist), directed = FALSE, vertices = map |> dplyr::select(local_id, score)
    )

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
  already_used <- c()
  improved_routes <- pbapply::pblapply(
    routing_results$agent_id,
    function(zone_id) {
      sr <- solve_routing(obj = "SDR", L = L, zone_id = zone_id)
      ir <- improve_routing(L_remaining = sr$L_remaining,
                            L,
                            sr$route,
                            zone_id)
      already_used <<- append(already_used, ir$route)
      # zones[[zone_id + 1]] <- c(1,zones[[zone_id + 1]][!zones[[zone_id + 1]] %in% already_used])
      try(zones[[zone_id + 1]] <<- c(1,zones[[zone_id + 1]][!zones[[zone_id + 1]] %in% already_used]))
      return(ir)
    }
  )

  # message("Trying to improve initial routes")
  # improved_routes <- pbapply::pblapply(
  #   routing_results$agent_id,
  #   function(zone_id) {improve_routing(
  #     L_remaining = initial_routes[[zone_id]]$L_remaining,
  #     route = initial_routes[[zone_id]]$route,
  #     # L = L - initial_routes[[zone_id]]$L_remaining/2,
  #     L = L,
  #     zone_id = zone_id
  #   )}
  # )

  # return(initial_routes)

  structure(
    list(
      # "initial_routes" = lapply(initial_routes, function(x) x$route),
      "improved_routes" = lapply(improved_routes, function(x) x$route),
      "L_remaining" = lapply(improved_routes, function(x) x$L_remaining),
      "total_score" = lapply(improved_routes, function(x) sum(inst$points$score[unique(x$route)])),
      "zones" = zones,
      "L" = L
    ),
    class = "starting_routes"
  )
}

no_zone_sr <- function(inst, L, k) {
  zones <- list()
  for (i in 1:k) zones[[i]] <- inst$points$id
  sr <- starting_routes(inst, zones, L)
  return(sr)
}

sr <- no_zone_sr(inst, L = 100, k = 3)

plot_route <- function(routes) {
  route_segments <- tibble::tibble(routes, agent_id = 1:length(routes)) |>
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
    ggplot2::geom_text(
      data = inst$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, label = id)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend, linetype = as.character(agent_id))
    ) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::labs(x = "x", y = "y", linetype = "Agent id")
}

plot_route(sr$improved_routes)

sr$L_remaining
