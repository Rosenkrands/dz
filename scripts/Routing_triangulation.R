library(deldir)
library(tidyverse)
library(dz)
library(igraph)
library(dplyr)

# For testing purposes:
# clust <- readRDS("clust_ls.rds")

all_points <- clust$instance$points |>
  dplyr::filter(id != nrow(clust$instance$points)) |>
  dplyr::select(id, x, y, score, zone)

select_zone <- function(zone_id) {
  # zone_id = 1
  clust$instance$points |>
    dplyr::filter((id == 1) | (zone == zone_id))
}

points <- 1:clust$k |> as.list() |> lapply(select_zone)

# Function for calculating the distance of the shortest (DL) path between 2 points.
dist <- function(id1, id2, g){
  # Find vertices that make up the path
  if (id1 == id2) return(0)
  short_vert <- as.vector(shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
  # Calculate total distance between them
  route_length <- 0
  dist_matrix <- distances(g)
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
  short_vert <- as.vector(shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
  return(short_vert)
}

g <- igraph::graph.data.frame(
  tri |> dplyr::select(ind1, ind2, weight = dist),
  directed = FALSE,
  vertices = clust$instance$points |> dplyr::select(id, score)
)

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
    score_temp_realized[placement] <- map$score_variance[placement]
  }
  for (placement in (1):(length(score_temp_expected)-1)) {
    score_temp_expected[placement] <- map$score[placement]
  }
  return(sum(score_temp_realized, na.rm = T) + sum(score_temp_expected, na.rm = T))
}


# Create route given points
solve_routing <- function(obj = 'SDR', L = 150, zone_id = 1){
  # obj = 'SDR'; L = 150; zone_id = 1
  L_remaining <- L
  map = clust$instance$points |>
    dplyr::filter((id == 1) | (zone == zone_id))

  delsgs <- clust$same_zone_edges |>
    dplyr::filter(zone == zone_id) |>
    as_tibble()

  delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

  # adapt to correct ids
  lookup <- map |> dplyr::mutate(local_id = row_number()) |> select(local_id, id)
  map <- map |> mutate(local_id = row_number(), .before = everything())
  delsgs <- delsgs |>
    dplyr::inner_join(lookup, by = c("ind1" = "id")) |>
    dplyr::select(-ind1, ind1 = local_id) |>
    dplyr::inner_join(lookup, by = c("ind2" = "id")) |>
    dplyr::select(-ind2, ind2 = local_id)

  g <- graph.data.frame(delsgs %>% select(ind1, ind2, weight = dist), directed = FALSE, vertices = map %>% select(local_id, score))
  candidates <- map$local_id
  route = integer()
  route <- append(route, 1)
  last_in_current <- route[length(route)]
  route <- append(route, 1)
  s_total <- 0
  while (L_remaining > 0) {
    print(lookup$id[route])
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
        SDR[1] <- 0
      }
      New_last <- which.max(SDR)
      all_short_path <- dist2(route[length(route)-1], New_last, g = g)
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
    while ((dist(last_in_current, New_last, g = g) + dist(New_last, 1, g = g) - dist(last_in_current,  1, g = g)) > L_remaining){
      SDR[New_last] <- 0
      New_last <- which.max(SDR)
      if (SDR[New_last] == 0) {
        # Add route back to base
        all_short_path_return <- dist2(route[(length(route)-1)], 1, g = g)
        route <- append(route, all_short_path_return[2:(length(all_short_path_return)-1)], after = length(route)-1)
        for (i in 1:length(route)){
          route_global[i] <- lookup$id[route[i]]
        }
        L_remaining <- L - route_length(route = route_global)
        # Function to plot path using information in route object
        output <- list("route" = route_global, "L_remaining" = L_remaining, "s_total" = s_total, "delsgs" = delsgs, "lookup" = lookup)
        return(output)
      }
    }
    route <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
    route_global <- vector(length = length(route))
    for (i in 1:length(route)){
      route_global[i] <- lookup$id[route[i]]
    }
    L_remaining <- L - route_length(route = route_global)
  }
}

# we want to create a route for each zone
routing_results <- tibble::tibble(agent_id = 1:clust$k)

# Caclculate the routes
rslt <- lapply(
  routing_results$agent_id,
  function(zone_id) {solve_routing(obj = "SDR", L = 150, zone_id = zone_id)}
)

# then we gather results from the k routes into one data structure
route_list <- lapply(
  rslt,
  function(arg) {arg$route} # convert from local_id to id
)

routing_results$routes <- route_list
routing_results$L <- do.call(c, lapply(rslt, function(arg) {arg$L}))
routing_results$s_total <- do.call(c, lapply(rslt, function(arg) {arg$s_total}))

rout <- structure(
  list(
    "routing_results" = routing_results,
    "obj" = obj,
    "L" = L,
    "clustering" = clust
  ),
  class = "routing"
)

plot.routing <- function(rout) {
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
      data = clust$same_zone_edges,
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
