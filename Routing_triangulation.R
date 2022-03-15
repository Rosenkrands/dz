library(deldir)
library(tidyverse)
library(dz)
library(igraph)

# Our data
terminal <- as.data.frame(test_instances$p7_chao$points %>% filter(point_type == "terminal") %>% select(-'point_type'))[1,]
points <- as.data.frame(test_instances$p7_chao$points %>% filter(point_type == "intermediate") %>% select(-'point_type'))
all_points <- rbind(terminal, points)

# Triangulation
tp <- deldir(x = all_points$x, y = all_points$y)
plot(tp)

tp$delsgs$dist <- sqrt((tp$delsgs$x1 - tp$delsgs$x2)^2 + (tp$delsgs$y1 - tp$delsgs$y2)^2)

# Convert to igraph
g <- graph.data.frame(tp$delsgs %>% select(ind1, ind2, weight = dist), directed = FALSE, vertices = all_points %>% select(id, score))
# V(g)$size <- V(g)$score
# E(g)$width <- E(g)$dist*0.2
plot(g)


### Delaunay insertion

# Distance function using ID
# Update to only use DT using shortest path algorithm
# dist <- function(id1, id2){
#   p1 <- test_instances$p7_chao$points[id1 == test_instances$p7_chao$points$id, c("x","y")]
#   p2 <- test_instances$p7_chao$points[id2 == test_instances$p7_chao$points$id, c("x","y")]
#   return(as.numeric(sqrt((p1[1] - p2[1])^2 + (p1[2]-p1[2])^2)))
# }

# Function for calculating the distance of the shortest (DL) path between 2 points.
dist <- function(id1, id2){
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
dist2 <- function(id1, id2){
  # Find vertices that make up the path
  if (id1 == id2) return(0)
  short_vert <- as.vector(shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]])
  return(short_vert)
}

# Create route given points
solve_routing <- function(obj = 'SDR', L = 100, map = all_points, plot = T){
  candidates <- map$id
  route = integer()
  route <- append(route, terminal$id[1])
  last_in_current <- route[length(route)]
  route <- append(route, terminal$id[1])
  s_total <- 0
  while (L > 0) {
    if (obj == 'SDR'){
      d <- vector(length = length(map$id))
      s <- vector(length = length(map$id))
      SDR <- vector(length = length(map$id))
      for (candidate in candidates) {
        route_temp <- route
        route_temp <- append(route_temp, candidate, after = length(route_temp)-1)
        d[candidate] <- dist(route[length(route)], candidate) +
          dist(candidate, route[length(route)-1]) -
          as.vector(dist(route[length(route_temp)-2], route_temp[1]))
        s[candidate] <- map[candidate,]$score
        SDR[candidate] <- s[candidate]/d[candidate]
      }
      New_last <- which.max(SDR)
      all_short_path <- dist2(route[length(route)-1], New_last)
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
      all_short_path <- dist2(route[length(route)-1], New_last)
      s_total <- s_total + map[New_last,]$score
      map[New_last,]$score <- 0
      print(New_last)
    }
    if (dist(last_in_current, New_last) + dist(New_last, terminal$id[1]) - dist(last_in_current,  terminal$id[1]) < L){
      route <- append(route, all_short_path[2:length(all_short_path)], after = length(route)-1)
      # For-loop to remove all new distances, not just the last in new shortest path
      L <- L + dist(last_in_current, terminal$id[1])
      L <- L - dist(route[length(route)], route[length(route)-1])
      if (length(all_short_path > 2)){
        for (i in 1:(length(all_short_path)-1)){
          L <- L - dist(all_short_path[length(all_short_path)-i+1], all_short_path[length(all_short_path)-i])
        }
      }
      # print(route)
    } else {
      # Switch last two before terminal
      # route <- replace(route, c(length(route)-1, length(route)-2), route[c(length(route)-2, length(route)-1)])
      # Function to plot path using information in route object
      if (plot == TRUE){
        plot_obj <- list()
        rout <- list()
        plot_obj[[1]] <- route
        rout$routes <- tibble(agent_id = 1:1, routes = plot_obj)
        rout$instance$points <- test_instances$p7_chao$points
        route_segments <- rout$routes |>
          tidyr::unnest(routes) |>
          dplyr::group_by(agent_id) |>
          dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
          dplyr::filter(!is.na(id_start)) |>
          dplyr::select(-routes) |>
          dplyr::inner_join(rout$instance$points |> dplyr::select(id, x, y),
                            by = c("id_start" = "id")) |>
          dplyr::inner_join(rout$instance$points |> dplyr::select(id, x, y),
                            by = c("id_end" = "id"), suffix = c("","end"))

        # Plot the segment on the existing plot
        plot <- ggplot2::ggplot() +
          # ggalt::geom_encircle(
          #   data = rout$in_points,
          #   ggplot2::aes(x = x, y = y, group = zone, fill = as.character(zone)),
          #   s_shape = 1, expand = 0, alpha = 0.2
          # ) +
          ggplot2::geom_text(
            data = rout$instance$points |> dplyr::filter(point_type == "intermediate"),
            ggplot2::aes(x, y, label = id)
          ) +
          ggplot2::geom_segment(
            data = route_segments,
            ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
          ) +
          ggplot2::geom_point(
            data = rout$instance$points |> dplyr::filter(point_type == "terminal"),
            ggplot2::aes(x, y), color = "red", shape = 17
          ) +
          ggplot2::ggtitle(paste0("Instance: ", rout$instance$name)) +
          ggplot2::theme_bw() +
          ggplot2::guides(
            shape = "none",
            fill = "none"
          ) +
        ggplot2::geom_segment(
          data = tri$delsgs,
          ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
          color = ggplot2::alpha("black", 0.3), linetype = "dashed"
        )
      }
      output <- list(route, L, s_total, plot)
      return(output)
    }
  }
}




