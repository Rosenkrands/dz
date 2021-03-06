library(tidyverse)
library(dz)
library(igraph)

# function to plot progress of routing
plot_progress <- function() {
  route_segments <- tibble::tibble(agent_id = zone_id, routes = original_route) |>
    # dplyr::mutate(routes = routes) |>
    # tidyr::unnest(routes) |>
    # dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(clust$instance$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  # route segments for the updated routes
  # routes[[zone_id]] <- route

  updated_route_segments <- tibble::tibble(agent_id = zone_id, routes = route) |>
    # dplyr::mutate(routes = routes) |>
    # tidyr::unnest(routes) |>
    # dplyr::group_by(agent_id) |>
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
    # ggplot2::geom_point(
    #   data = clust$instance$points |> dplyr::filter(id %in% candidates),
    #   ggplot2::aes(x, y), color = "blue",
    #   shape = 21, stroke = 1
    # ) +
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

# Function for calculating the distance of the shortest (DL) path between 2 points.
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


### Realization of nearby score values
# Using the variance (and score) columns
# clust <- readRDS("clust_ls.rds"); obj = "SDR"; L = 200
#
# variances <- generate_variances(clust$instance)
#
# map <- clust$instance$points |>
#   dplyr::select(-score_variance) |>
#   dplyr::left_join(variances, by = c("id")) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(realized_score = ifelse(is.na(score_variance), NA, rnorm(1, mean = score, sd = sqrt(score_variance)))) |>
#   dplyr::mutate(unexpected = purrr::rbernoulli(1, p = p_unexpected))
#   # dplyr::mutate(unexpected = (realized_score > (score+1.96*sqrt(score_variance))) | (realized_score < (score-1.96*sqrt(score_variance))))
# map$realized_score[1] <- 0
# map$unexpected[1] <- FALSE
# edges <- clust$same_zone_edges |> dplyr::filter(zone == zone_id)
# # These are only applied/updated when we are within some distance determined at each node according to Kaspers distance to rectangle function
#
# # Compute edges in delaunay triangulation
# tri <- (deldir::deldir(clust$instance$points$x, clust$instance$points$y))$delsgs
#
# # construct the igraph object
# tri$dist <- sqrt((tri$x1 - tri$x2)^2 + (tri$y1 - tri$y2)^2)
#
# g <- igraph::graph.data.frame(
#   tri |> dplyr::select(ind1, ind2, weight = dist),
#   directed = FALSE,
#   vertices = clust$instance$points |> dplyr::select(id, score)
# )

# Solution information
# route_info <- solve_routing(L=300)
# route <- route_info$route
# edges <- tri

# Convert to global id
# for (i in 1:length(edges$ind1)){
#   edges$ind1[i] <- route_info$lookup$id[edges$ind1[i]]
# }
# for (i in 1:length(edges$ind2)){
#   edges$ind2[i] <- route_info$lookup$id[edges$ind2[i]]
# }


#

### New function purely for updating the path when an alternative route becomes better
### due to deviation in realized_score compared to expected score

initial_route = rout$routing_results$routes[[4]]; zone_id = 4; L = rout$L; L_remaining = rout$routing_results$L[4]
update_routing <- function(initial_route, zone_id, L, L_remaining) {
  clust <- readRDS("clust_ls.rds")
  variances <- generate_variances(clust$instance)

  map <- clust$instance$points |>
    dplyr::select(-score_variance) |>
    dplyr::left_join(variances, by = c("id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(realized_score = ifelse(is.na(score_variance), NA, rnorm(1, mean = score, sd = sqrt(score_variance)))) |>
    dplyr::mutate(unexpected = purrr::rbernoulli(1, p = p_unexpected))
  # dplyr::mutate(unexpected = (realized_score > (score+1.96*sqrt(score_variance))) | (realized_score < (score-1.96*sqrt(score_variance))))
  map$realized_score[1] <- 0
  map$unexpected[1] <- FALSE

  delsgs <- clust$same_zone_edges |>
    dplyr::filter(zone == zone_id) |>
    as_tibble()

  delsgs$dist <- sqrt((delsgs$x1 - delsgs$x2)^2 + (delsgs$y1 - delsgs$y2)^2)

  # Get vector of node ids in zone
  zone <- c(1,map |> dplyr::filter(zone == zone_id) |> dplyr::pull(id))
  # g <- igraph::induced_subgraph(test_instances$p7_chao$g, vids = zone)
  g <- graph.data.frame(delsgs %>% select(ind1, ind2, weight = dist), directed = FALSE, vertices = map %>% select(id, score))
  dst <- igraph::distances(g)
  # L <- 150
  # initial_route <- solve_routing(L = L)
  # remaining_route <- initial_route$route
  remaining_route <- initial_route
  remaining_nodes <- c(remaining_route[3:length(remaining_route)])
  route <- remaining_route[1:2]
  print(plot_progress())
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
    L_remaining <- L - route_length(route = route)
    L_required <- dist(id_next, New_point, dst = dst) + dist(New_point, remaining_nodes[2], dst = dst) + dist(remaining_nodes[2], 1, dst = dst)
    while (L_remaining < L_required) {
      SDR_cand[New_point] <- 0
      New_point <- which.max(SDR_cand)
      L_required <- dist(id_next, New_point, dst = dst) + dist(New_point, remaining_nodes[2], dst = dst)
      if (New_point == 1) {break}
    }
    # if (remaining_route[1] == remaining_route[3]) {remaining_route <- remaining_route[3:length(remaining_route)]}
    cat("Remaining length:", "\n")
    print(L_remaining)
    if ((max(SDR_cand) > SDR_planned_realized)  & !(New_point %in% remaining_route) & (L_remaining > L_required) ){
      # Remove the node that would originally be visited after id_next
      map$realized_score[New_point] <- 0
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
      # Go where we would anyway
      route <- append(route, remaining_route[3])
      # Update remaining_route by removing the ones already visited (excluding id_now and id_next for the next iteration)
      remaining_route <- remaining_route[2:(length(remaining_route))]
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
  print(plot_progress())
  output <- list("route" = route, "s_total" = route_score(route, id_next_placement = length(route)))
  return(output)
}
# TEST for one zone at a time
z <- 2
update_routing(initial_route = rout$routing_result$routes[[z]], L = rout$L, L_remaining = rout$routing_results$L[z], zone_id = z)

# Use lapply to perform update for all clusters
updated_route_list <- lapply(
  rout$routing_result$agent_id,
  function(zone_id) {update_routing(initial_route = rout$routing_result$routes, L = rout$L, L_remaining = rout$routing_results$L, zone_id = zone_id)}
)

route_list <- lapply(
  rslt,
  function(arg) {arg$route} # convert from local_id to id
)



plot_progress()







