#' Generate routes for a clustered instance
#'
#' Given a clustered instance and a routing method the function will provide routes for the given instance.
#'
#' @param clust A list returned from the `clustering` function
#' @param obj The objective to use for the routing
#' @param L The range constraint induced on the agents
#'
#' @return A list
#' @export
#'
routing <- function(clust, obj = "SDR", L = 300, variances) {
  # For testing purposes:
  # clust <- readRDS("clust_ls.rds"); obj = "SDR"; L = 500; variances = generate_variances(inst = clust$instance)

  # reuse igraph created during clustering
  g <- clust$g

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

  # Create route given points
  solve_routing <- function(obj = obj, L = L, zone_id = 1){
    # obj = obj; L = 100; zone_id = 1
    map = clust$instance$points |>
      dplyr::filter((id == 1) | (zone == zone_id))

    delsgs <- clust$same_zone_edges |>
      dplyr::filter(zone == zone_id) |>
      tibble::as_tibble()

    delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

    # adapt to correct ids
    lookup <- map |> dplyr::mutate(local_id = dplyr::row_number()) |> dplyr::select(local_id, id)
    map <- map |> dplyr::mutate(local_id = dplyr::row_number(), .before = dplyr::everything())

    delsgs <- delsgs |>
      dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
      dplyr::select(-ind1, ind1 = local_id) |>
      dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
      dplyr::select(-ind2, ind2 = local_id)

    g <- igraph::graph.data.frame(
      delsgs |> dplyr::select(ind1, ind2, weight = dist),
      directed = FALSE,
      vertices = map |> dplyr::select(local_id, score)
    )

    candidates <- map$local_id
    route = integer()
    route <- append(route, 1)
    last_in_current <- route[length(route)]
    route <- append(route, 1)
    s_total <- 0
    while (L > 0) {
      if (obj == 'SDR'){
        d <- vector(length = length(map$id))
        s <- vector(length = length(map$id))
        SDR <- vector(length = length(map$id))
        for (i in 1:length(candidates)) {
          route_temp <- route
          route_temp <- append(route_temp, candidates[i], after = length(route_temp)-1)
          d[i] <- dist(route[length(route)], candidates[i], g = g) +
            dist(candidates[i], route[length(route)-1], g = g) -
            as.vector(dist(route[length(route_temp)-2], route_temp[1], g = g))
          s[i] <- map[candidates[i],]$score
          SDR[i] <- s[i]/d[i]
        }
        New_last <- which.max(SDR)
        all_short_path <- dist2(route[length(route)-1], New_last, g = g)
        # print(all_short_path[2:length(all_short_path)])
        #print(route)
        # candidates <- candidates[!candidates %in% all_short_path]
        for (node in (all_short_path[2:length(all_short_path)])) {
          s_total <- s_total + map[node,]$score
          map[node,]$score <- 0
        }
      }
      if (obj == 'random'){
        New_last <- sample(2:101, size = 1)
        all_short_path <- dist2(route[length(route)-1], New_last, g = g)
        s_total <- s_total + map[New_last,]$score
        map[New_last,]$score <- 0
        print(New_last)
      }
      if (dist(last_in_current, New_last, g = g) + dist(New_last, 1, g = g) - dist(last_in_current,  1, g = g) < L){
        route <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
        all_short_path_return <- dist2(New_last, 1, g = g)
        # For-loop to remove all new distances, not just the last in new shortest path
        L <- L + dist(last_in_current, 1, g = g)
        L <- L - dist(route[length(route)], route[length(route)-1], g = g)
        if (length(all_short_path > 2)){
          for (i in 1:(length(all_short_path)-1)){
            L <- L - dist(all_short_path[length(all_short_path)-i+1], all_short_path[length(all_short_path)-i], g = g)
          }
        }
        # print(route)
      } else {
        route <- append(route, all_short_path_return[2:(length(all_short_path_return)-1)], after = length(route)-1)
        # Switch last two before terminal
        # route <- replace(route, c(length(route)-1, length(route)-2), route[c(length(route)-2, length(route)-1)])
        # Function to plot path using information in route object
        output <- list("route" = route, "L" = L, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
        return(output)
      }
    }
  }

  # we want to create a route for each zone
  routing_results <- tibble::tibble(agent_id = 1:clust$k)

  # calculate the routes
  initial_routes <- lapply(
    routing_results$agent_id,
    function(zone_id) {solve_routing(obj = "SDR", L = L, zone_id = zone_id)}
  )

  initial_routes_list <- lapply(
    initial_routes,
    function(arg) {arg$lookup$id[arg$route]} # convert from local_id to id
  )

  # function to plot progress of routing
  plot_progress <- function() {
    route_segments <- tibble::tibble(agent_id = 1:clust$k) |>
      dplyr::mutate(routes = routes) |>
      tidyr::unnest(routes) |>
      dplyr::group_by(agent_id) |>
      dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
      dplyr::filter(!is.na(id_start)) |>
      dplyr::select(-routes) |>
      dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                        by = c("id_start" = "id")) |>
      dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                        by = c("id_end" = "id"), suffix = c("","end"))

    # route segments for the updated routes
    routes[[zone_id]] <- route

    updated_route_segments <- tibble::tibble(agent_id = 1:clust$k) |>
      dplyr::mutate(routes = routes) |>
      tidyr::unnest(routes) |>
      dplyr::group_by(agent_id) |>
      dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
      dplyr::filter(!is.na(id_start)) |>
      dplyr::select(-routes) |>
      dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                        by = c("id_start" = "id")) |>
      dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                        by = c("id_end" = "id"), suffix = c("","end"))


    # Plot the segment on the existing plot
    ggplot2::ggplot() +
      ggplot2::geom_segment(
        data = clust$same_zone_edges,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = clust$instance$points |> dplyr::filter(id == id_next),
        ggplot2::aes(x, y), color = "green",
        shape = 21, size = 6, stroke = 2
      ) +
      ggplot2::geom_point(
        data = clust$instance$points |> dplyr::filter(id %in% candidates),
        ggplot2::aes(x, y, size = SDR[candidates]), color = "blue",
        shape = 21, stroke = 1
      ) +
      # Plot points and dots
      # ggplot2::geom_point(
      #   data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
      #   ggplot2::aes(x, y, color= as.character(zone))
      # ) +
      # Plot points as ids
      ggplot2::geom_text(
        data = clust$instance$points |> dplyr::filter(point_type == "intermediate"),
        ggplot2::aes(x, y, label = id)
      ) +
      ggplot2::geom_segment(
        data = updated_route_segments,
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
        linetype = "solid", color = "blue"
      ) +
      ggplot2::geom_segment(
        data = route_segments,
        ggplot2::aes(x=x, y=y, xend=xend, yend=yend),
        linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = clust$instance$points |> dplyr::filter(point_type == "terminal"),
        ggplot2::aes(x, y), color = "red", shape = 17
      ) +
      ggplot2::ggtitle(paste0("Instance: ", clust$instance$name)) +
      ggplot2::theme_bw() +
      ggplot2::guides(
        shape = "none",
        fill = "none",
        color = "none",
        size = "none",
      )
  }


  update_routing <- function(r = 10, zone_id = 1) {
    # r = 10; zone_id = 1
    sub_g <- igraph::induced_subgraph(g, vids = clust$cl$zones[[zone_id]])

    ### Function for route length
    route_length <- function(route) {
      distance_temp <- vector(length = length(route)-1)
      for (placement in (1):(length(route)-1)) {
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
        score_temp_realized[placement] <- map$realized_score[placement]
      }
      for (placement in (1):(length(score_temp_expected)-1)) {
        score_temp_expected[placement] <- map$score[placement]
      }
      return(sum(score_temp_realized, na.rm = T) + sum(score_temp_expected, na.rm = T))
    }

    # map to stick with current notation
    map <- clust$instance$points |>
      dplyr::rowwise() |>
      dplyr::mutate(realized_score = ifelse(is.na(score_variance), NA, rnorm(1, mean = score, sd = sqrt(score_variance))))
    edges <- clust$same_zone_edges |> dplyr::filter(zone == zone_id)

    route <- initial_routes_list[[zone_id]]
    cat("Starting the route updating loop...\n")
    node_nr = 0
    while (!is.na(route[node_nr+2])) {
      node_nr <- node_nr +1
    # }
    # for (node_nr in 1:(length(route)-2)){
      # Get nodes with edges to this node
      id_now <- route[node_nr]; cat("id_now is", id_now)
      id_next <- route[node_nr+1]; cat("\tid_next is", id_next, "\n")
      #if (is.na(id_next)) {break}
      # cat("id_next is:", id_next, "\n"); if (id_next == 27) stop()
      cat(route, "\n")
      map$realized_score[id_next] <- 0
      current_line <- edges |> dplyr::filter(ind1 == id_now | ind1 == id_next, ind2 == id_now | ind2 == id_next)
      remaining_nodes <- route[(node_nr+2):(length(route))]
      l <- 0
      dist_to_edge <- vector()
      candidates <- integer(0)
      for (node in unique(clust$cl$zones[[zone_id]])) {
        if (node != id_next) {
          #Get their coordinates
          l <- l+1
          if (node %in% edges$ind1){
            point <- unique(edges |> dplyr::filter(ind1 == node) |> dplyr::select(x1, y1))
          } else {
            point <- unique(edges |> dplyr::filter(ind2 == node) |> dplyr::select(x1 = x2, y1 = y2))
          }
          dist_to_edge[l] <- distancePointSegment(px = point$x1, py <- point$y1, x1 = current_line$x1, x2 = current_line$x2, y1 = current_line$y1, y2 = current_line$y2)
          if (dist_to_edge[l] < r){
            # Nodes on path within viewing distance
            candidates <- append(candidates, node)
          }
        }
      }
      # Use the candidates to evaluate different routes, loop for all possible:
      # 1. Length of new route
      # 3. Trade-off
      #g <- graph.data.frame(delsgs %>% select(ind1, ind2, weight = dist), directed = FALSE, vertices = all_points %>% select(local_id, score))
      s_total <- 0
      d <- vector(length = length(map$id))
      s <- vector(length = length(map$id))
      SDR <- vector(length = length(map$id))
      for (i in 1:length(candidates)) {
        route_temp <- route
        route_temp <- append(route_temp, candidates[i], after = match(id_next, route))
        route_temp <- route_temp[-(match(id_next, route_temp)+2)]
        # d[i] <- dist(route[length(route)], candidates[i], g = g) +
        #   dist(candidates[i], id_next, g = g)
        d[i] <- route_length(route = route_temp)
        # Realized score
        # s[i] <- (map$score_variance)[candidates[i]]
        s[i] <- route_score(route = route_temp, id_next_placement = node_nr + 1)
        # Updated SDR
        SDR[candidates[i]] <- s[i]/d[i]
      }
      New_point <- which.max(SDR)
      # Chose best new route if it is better than original
      d_temp <- vector()
      s_temp <- vector()
      for (i in (match(id_next, route)):((length(route))-1)){
        d_temp[i] <- dist(route[i], route[i+1], g = g)
        s_temp[i] <- (map$score)[route[i+1]]
      }
      d_expected <- sum(d_temp, na.rm = T)
      s_expected <- sum(s_temp, na.rm = T)
      SDR_expected <- s_expected/d_expected
      # if (id_next == 14) {stop()}
      if (max(SDR, na.rm = TRUE) > SDR_expected){
        # Connect to the remainder of original path
        new_all_short_path <- dist2(id_next, New_point, g = g)
        if (new_all_short_path == 0) {next}
        new_all_short_path <- new_all_short_path[2:(length(new_all_short_path))]
        route <- route[-(match(id_next, route)+1)]
        after <- match(id_next, route)
        route <- append(route, new_all_short_path, after = after)
        if (route[after + 1] == route[after + 2]) route <- route[-(after + 1)]
        for (node in (new_all_short_path)) {
          s_total <- s_total + map[node,]$score
          map[node,]$score <- 0
        }
      }
    }
    return(route)
  }

  updated_routes <- lapply(
    routing_results$agent_id,
    function(zone_id) {update_routing(zone_id = zone_id)}
  )

  # update routes based on realized values from the uncertainty
  map <- clust$instance$points %>%
    dplyr::left_join(variances, by = c("id"))



  # then we gather results from the k routes into one data structure

  routing_results$routes <- updated_routes
  routing_results$L <- do.call(c, lapply(rslt, function(arg) {arg$L}))
  routing_results$s_total <- do.call(c, lapply(rslt, function(arg) {arg$s_total}))

  structure(
    list(
      "routing_results" = routing_results,
      "obj" = obj,
      "L" = L,
      "clustering" = clust
    ),
    class = "routing"
  )
}

#' Plot method for a routing object
#'
#' Visualize the generated routes
#'
#' @param rout A list returned from the `routing` function
#'
#' @return A ggplot object
#' @export
#'
plot.routing <- function(rout) {
  # Find the segments induced by the routes
  route_segments <- rout$routing_results |>
    tidyr::unnest(routes) |>
    dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(rout$clustering$instance$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(rout$clustering$instance$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  # Plot the segment on the existing plot
  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = rout$clustering$same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = rout$clustering$instance$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = rout$clustering$instance$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color= as.character(zone))
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
    ) +
    ggplot2::ggtitle(paste0("Instance: ", rout$clustering$instance$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none"
    )
}
