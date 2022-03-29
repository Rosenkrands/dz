library(tidyverse)
library(dz)
set.seed(10)

zone_id = 1

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


### Realization of nearby score values
# Using the variance (and score) columns
clust <- readRDS("clust_ls.rds"); obj = "SDR"; L = 500
map <- clust$instance$points |>
  dplyr::rowwise() |>
  dplyr::mutate(realized_score = ifelse(is.na(score_variance), NA, rnorm(1, mean = score, sd = sqrt(score_variance))))
map$realized_score[1] <- 0
edges <- clust$same_zone_edges |> dplyr::filter(zone == zone_id)
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
# route_info <- solve_routing(L=300)
# route <- route_info$route
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
r <- 100
# 1. Calculate distance from current line segment to other nodes
# for (node_nr in 1:(length(route)-2)){
#   # Get nodes with edges to this node
#   id_now <- route[node_nr]
#   id_next <- route[node_nr+1]
#   if (id_now == id_next) {
#     node_nr = node_nr + 1
#     id_now <- route[node_nr]
#     id_next <- route[node_nr+1]
#   }
#   print(id_next)
#   map$realized_score[id_next] <- 0
#   map$score[id_next] <- 0
#   current_line <- edges %>% dplyr::filter(ind1 == id_now | ind1 == id_next, ind2 == id_now | ind2 == id_next)
#   #remaining_nodes <- route[(node_nr+2):(length(route))]
#   nodes_in_zone <- (map %>% filter(zone == 1))$id
#   l <- 0
#   dist_to_edge <- vector()
#   candidates <- integer(0)
#   for (node in nodes_in_zone) {
#     #Get their coordinates (nodes_in_zone)
#     l <- l+1
#     if (node %in% edges$ind1){
#       point <- unique(edges %>% filter(ind1 == node) %>% select(x1, y1))
#     } else {
#       point <- unique(edges %>% filter(ind2 == node) %>% select(x1 = x2, y1 = y2))
#     }
#     dist_to_edge[l] <- distancePointSegment(px = point$x1, py <- point$y1, x1 = current_line$x1, x2 = current_line$x2, y1 = current_line$y1, y2 = current_line$y2)
#     if (dist_to_edge[l] < r){
#       # Nodes on path within viewing distance
#       candidates <- append(candidates, node)
#     }
#   }
#   # Use the candidates to evaluate different routes, loop for all possible:
#   # 1. Length of new route
#   # 3. Trade-off
#   #g <- graph.data.frame(delsgs %>% select(ind1, ind2, weight = dist), directed = FALSE, vertices = all_points %>% select(local_id, score))
#   s_total <- 0
#   d <- vector(length = length(map$id))
#   s <- vector(length = length(map$id))
#   SDR <- vector(length = length(map$id))
#   for (i in 1:length(candidates)) {
#     route_temp <- route
#     route_temp <- append(route_temp, candidates[i], after = match(id_next, route))
#     route_temp <- route_temp[-(match(id_next, route_temp)+2)]
#     # d[i] <- dist(route[length(route)], candidates[i], g = g) +
#       # dist(candidates[i], id_next, g = g)
#     d[i] <- route_length(route = route_temp)
#     # Realized score
#     # s[i] <- (map$score_variance)[candidates[i]]
#     s[i] <- route_score(route = route_temp, id_next_placement = node_nr + 1)
#     # Updated SDR
#     SDR[candidates[i]] <- s[i]/d[i]
#   }
#   SDR[id_next] <- 0
#   New_point <- which.max(SDR)
#   # Chose best new route if it is better than original
#   d_temp <- vector()
#   s_temp <- vector()
#   for (i in (match(id_next, route)):((length(route))-1)){
#     d_temp[i] <- dist(route[i], route[i+1], g = g)
#     s_temp[i] <- (map$realized_score)[route[i+1]]
#   }
#   d_expected <- sum(d_temp, na.rm = T)
#   s_expected <- sum(s_temp, na.rm = T)
#   SDR_expected <- s_expected/d_expected
#   if (max(SDR, na.rm = TRUE) > SDR_expected){
#     # Connect to the remainder of original path
#     new_all_short_path <- dist2(id_next, New_point, g = g)
#     new_all_short_path <- new_all_short_path[2:(length(new_all_short_path))]
#     route <- route[-(match(id_next, route)+1)]
#     route <- append(route, new_all_short_path, after = match(id_next, route))
#     for (node in (new_all_short_path)) {
#       s_total <- s_total + map[node,]$score
#       map[node,]$score <- 0
#     }
#   }
# }
#



### New function purely for updating the path when an alternative route becomes better
### due to deviation in realized_score compared to expected score

remaining_route <- c(1, 40, 42, 63, 85, 14, 22, 1)
remaining_nodes <- remaining_route[3:length(remaining_route)]
route <- remaining_route[1:2]
while(length(remaining_nodes) != 0){
  # Keep track of changes
  last_remaining_route <- remaining_route
  # Node the UAV flew from
  id_now <- remaining_route[1]
  print(id_now)
  # The node currently occupied by the UAV
  id_next <- remaining_route[2]
  print(id_next)
  # Since it has been visited the score is updated
  map$realized_score[id_now] <- 0
  map$realized_score[id_next] <- 0
  map$score[id_next] <- 0
  # The previous path traveled to get here
  current_line <- edges %>% dplyr::filter(ind1 == id_now | ind1 == id_next, ind2 == id_now | ind2 == id_next)
  # Viewing distance to all nodes in the zones
  nodes_in_zone <- (map %>% filter(zone == 1))$id
  l <- 0
  dist_to_edge <- vector()
  candidates <- integer(0)
  for (node in nodes_in_zone) {
    #Get their coordinates (nodes_in_zone)
    l <- l+1
    if (node %in% edges$ind1){
      point <- unique(edges %>% filter(ind1 == node) %>% select(x1, y1))
    } else {
      point <- unique(edges %>% filter(ind2 == node) %>% select(x1 = x2, y1 = y2))
    }
    dist_to_edge[l] <- distancePointSegment(px = point$x1, py <- point$y1, x1 = current_line$x1, x2 = current_line$x2, y1 = current_line$y1, y2 = current_line$y2)
    if (dist_to_edge[l] < r){
      # Nodes in zone within viewing distance
      candidates <- append(candidates, node)
    }
  }
  # Evaluate how good the next planned node to be visited is when using realized score
  d_planned_realized <- dist(id_next, remaining_nodes[1],  g = g) +
    dist(remaining_nodes[1], remaining_nodes[2], g = g)
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
    print(candidate)
    # From current to candidate
    d_cand_1[candidate] <- dist(id_next, candidate, g = g)
    sp_cand_1[[candidate]] <- dist2(id_next, candidate, g = g)
    # Add score for points visited
    for (sp_node in (sp_cand_1[[candidate]])) {
      s_cand_tot[candidate] <- s_cand_tot[candidate] + map$realized_score[sp_node]
    }
    # From candidate to remainder of original route
    d_cand_2[candidate] <- dist(candidate, remaining_nodes[2], g = g)
    sp_cand_2[[candidate]] <- dist2(candidate, remaining_nodes[2], g = g)
    for (sp_node in sp_cand_2[[candidate]]) {
      # handle the case of candidate being equal to remaining_nodes[2]
      realized_score <- map$realized_score[sp_node]
      if (length(realized_score) == 0) {realized_score <- map$realized_score[candidate]}

      s_cand_tot[candidate] <- s_cand_tot[candidate] + realized_score
    }
    # Summarized
    d_cand_tot[candidate] <- d_cand_1[candidate] + d_cand_2[candidate]
    SDR_cand[candidate] <- (s_cand_tot[candidate])/(d_cand_tot[candidate])
  }
  New_point <- which.max(SDR_cand)
  # Add and remove these from the route according to (shortest paths) SDR
  # We remove two and add at least two
  longer_than_original <- 0
  if (max(SDR_cand) > SDR_planned_realized){
    # Remove the node that would originally be visited after id_next
    remaining_route <- remaining_route[remaining_route != remaining_nodes[1]]
    # Add new
    sp <- c(dist2(id_next, New_point, g = g)[2:(length(dist2(id_next, New_point, g = g)))],
            (dist2(New_point, remaining_nodes[2], g = g)[2:((length(dist2(New_point, remaining_nodes[2], g = g))))]))
    # Remove the extra start of end of original
    remaining_route <- remaining_route[remaining_route != (remaining_nodes[2])]
    remaining_route <- append(remaining_route, sp, after = 2)
    longer_than_original <- longer_than_original + (length(remaining_route) - length(last_remaining_route))
    if (is.na(remaining_route[3])) {route <- append(route, c(remaining_route[2], 1)); break}
    route <- append(route, remaining_route[3:(length(sp)+2)])
    remaining_route <- remaining_route[-(1:(length(sp)))]
  } else {
    if (is.na(remaining_route[3])) {route <- append(route, c(remaining_route[2], 1)); break}
    route <- append(route, remaining_route[3])
    # Update remaining_route by removing the ones already visited (excluding id_now and id_next for the next iteration)
    remaining_route <- remaining_route[remaining_route != remaining_route[1]]
  }
  # Update remaining_nodes
  remaining_nodes <- remaining_nodes[remaining_nodes != remaining_nodes[1]]
}

# ro$clustering$plot_data$zones[[1]][[1]] <- solve_routing(L = 500)$route
# routes <- ro$clustering$plot_data$zones[[1]]
# routes[[4]] <- 1
# routes[[1]] <- route_info$route
# plot_progress()





