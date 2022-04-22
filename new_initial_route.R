library(dz)
set.seed(1)

# Setting parameters
inst <- test_instances$p7_chao
L <- 100
variances <- generate_variances(inst)
info <- generate_information(inst)

# Prepare instance
p_inst <- prepare_instance(inst, variances, info)
(p <- plot(p_inst))

# Make copies of variables to alter during route generation
score <- p_inst$points$score
realized_score <- p_inst$points$realized_score
unexpected <- p_inst$points$unexpected

# reuse igraph created during clustering
g <- inst$g
dst <- inst$dst

# Dist function that returns only the points in the path
sp <- function(id1, id2, graph = g){
  # handle identical ids
  if (id1 == id2) {
    warning("Trying to calculate the shortest path from one node to itself, returning 0")
    return(0)
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

# initalize route vector
route <- c(1); current_node <- tail(route, 1)
L_remaining <- L
route_concluded <- F

get_SDR <- function(current_node, L_remaining, score) {
  # current_node = 1

  # The shortest paths to all node
  paths <- igraph::shortest_paths(
    g, from = as.character(current_node), to = igraph::V(g)
  )$vpath

  # The gathered profit from a path
  s <- do.call(
    c,
    lapply(
      paths,
      function(x) score[x |> names() |> as.integer()] |> sum()
    )
  )

  # The distance of a path
  d <- dst[current_node, ]

  # can we get to a node and back to source
  feasible <- d + dst[,1] <= L_remaining # TODO: we should maybe adjust L_remaining here to discourage paths that are close the L_remaining

  # set the infeasible nodes to 0 including the current node
  r <- s/d * feasible; r[is.na(r) | !is.finite(r)] <- 0

  # return SDR for the feasible nodes
  r[(r > 0) & names(r) != "1"]
}

# iterate this part

# Decide on the next node
sdr <- get_SDR(current_node, L_remaining, score)
candidates <- sdr |> names() |> as.integer()

candidate_points <- ggplot2::geom_point(
  data = inst$points[candidates,],
  ggplot2::aes(x,y), size = sdr, color = "green", alpha = .3
); p + candidate_points

if (length(candidates) > 1) { # there are multiple candidates
  node_id <- sample(candidates, 1, prob = sdr)
} else if (length(candidates) == 1) { # there is only one candidate
  node_id <- candidates[1]
} else if (length(candidates) < 1) { # there are no feasible candidates
  node_id <- 1; route_concluded <- T
}

node_id_point <- ggplot2::geom_point(
  data = inst$points |> dplyr::filter(id == node_id),
  ggplot2::aes(x,y)
); p + candidate_points + node_id_point

# Find path to next and append to route
path_to_next <- sp(current_node, node_id)
route <- append(route, path_to_next)

# Generate route segements based on the route
route_segments <- tibble::tibble(route = route) |>
  dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-route) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end")) |>
  dplyr::group_by(x,y,xend,yend)

route_to_node_id <- ggplot2::geom_segment(
  data = route_segments,
  ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
); p + node_id_point + route_to_node_id

p <- p + route_to_node_id

# Update variables
score[path_to_next] <- 0
L_remaining <- L_remaining - dst[current_node, node_id]

# check if anything was unexpected and update the correlated scores
for (j in path_to_next) { # we need to consider all nodes in the shortest path
  if (unexpected[j]) {
    related_nodes <- which(info[j,] != 0) # find the nodes that are related
    for (k in related_nodes) { # update score
      # TODO: What if the score have already been gather do we want to add new score or ignore points that are already visited?
      # Only update the scores for unvisited points
      score[k] <- score[k] + info[j,k]
    }
    unexpected[j] <- F
  }
}

current_node <- tail(route, 1)
route
