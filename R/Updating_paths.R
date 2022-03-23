library(tidyverse)
library(dz)

### Realization of nearby score values
# Using the variance (and score) columns
clust <- readRDS("clust_ls.rds"); obj = "SDR"; L = 500
map <- clust$instance$points
# These are only applied/updated when we are within some distance determined at each node according to Kaspers distance to rectangle function

# Compute edges in delaunay triangulation
tri <- (deldir::deldir(clust$instance$points$x, clust$instance$points$y))$delsgs

# construct the igraph object
tri$dist <- sqrt((tri$x1 - tri$x2)^2 + (tri$y1 - tri$y2)^2)

g <- igraph::graph.data.frame(
  tri |> dplyr::select(ind1, ind2, weight = dist),
  directed = FALSE,
  vertices = clust$instance$points |> dplyr::select(id, score)
)

# Solution information
route_info <- solve_routing(L=300)
route <- route_info$route
edges <- tri

# Convert to global id
# for (i in 1:length(edges$ind1)){
#   edges$ind1[i] <- route_info$lookup$id[edges$ind1[i]]
# }
# for (i in 1:length(edges$ind2)){
#   edges$ind2[i] <- route_info$lookup$id[edges$ind2[i]]
# }

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


# Use these in the loop below to improve SDR for the path as actual score is discovered

### Changes to the path
# Evaluation of whether the existing path is worth updating
r <- 1000
# 1. Calculate distance from current line segment to other nodes
for (node_nr in 1:(length(route)-2)){
  # Get nodes with edges to this node
  id_now <- route[node_nr]
  id_next <- route[node_nr+1]
  if (id_now == id_next) {
    node_nr = node_nr + 1
    id_now <- route[node_nr]
    id_next <- route[node_nr+1]
  }
  print(id_next)
  map$score_variance[id_next] <- 0
  map$score[id_next] <- 0
  current_line <- edges %>% dplyr::filter(ind1 == id_now | ind1 == id_next, ind2 == id_now | ind2 == id_next)
  #remaining_nodes <- route[(node_nr+2):(length(route))]
  nodes_in_zone <- (map %>% filter(zone == 1))$id
  l <- 0
  dist_to_edge <- vector()
  candidates <- integer(0)
  for (node in nodes_in_zone) {
    #Get their coordinates
    l <- l+1
    if (node %in% edges$ind1){
      point <- unique(edges %>% filter(ind1 == node) %>% select(x1, y1))
    } else {
      point <- unique(edges %>% filter(ind2 == node) %>% select(x1 = x2, y1 = y2))
    }
    dist_to_edge[l] <- distancePointSegment(px = point$x1, py <- point$y1, x1 = current_line$x1, x2 = current_line$x2, y1 = current_line$y1, y2 = current_line$y2)
    if (dist_to_edge[l] < r){
      # Nodes on path within viewing distance
      candidates <- append(candidates, node)
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
      # dist(candidates[i], id_next, g = g)
    d[i] <- route_length(route = route_temp)
    # Realized score
    # s[i] <- (map$score_variance)[candidates[i]]
    s[i] <- route_score(route = route_temp, id_next_placement = node_nr + 1)
    # Updated SDR
    SDR[candidates[i]] <- s[i]/d[i]
  }
  SDR[id_next] <- 0
  New_point <- which.max(SDR)
  # Chose best new route if it is better than original
  d_temp <- vector()
  s_temp <- vector()
  for (i in (match(id_next, route)):((length(route))-1)){
    d_temp[i] <- dist(route[i], route[i+1], g = g)
    s_temp[i] <- (map$score_variance)[route[i+1]]
  }
  d_expected <- sum(d_temp, na.rm = T)
  s_expected <- sum(s_temp, na.rm = T)
  SDR_expected <- s_expected/d_expected
  if (max(SDR, na.rm = TRUE) > SDR_expected){
    # Connect to the remainder of original path
    new_all_short_path <- dist2(id_next, New_point, g = g)
    new_all_short_path <- new_all_short_path[2:(length(new_all_short_path))]
    route <- route[-(match(id_next, route)+1)]
    route <- append(route, new_all_short_path, after = match(id_next, route))
    for (node in (new_all_short_path)) {
      s_total <- s_total + map[node,]$score
      map[node,]$score <- 0
    }
  }
}











