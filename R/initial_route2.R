#' Generation of initial routes as part of pre-mission cluster - 2nd edition
#'
#' This functions will the next node among all nodes relative to SDR as opposed to the first edition that sample only the first point.
#'
#' @param p_inst A list returned from the `prepare_instance` function
#' @param L The range for each agent
#' @param info An information matrix returned by the `generate_information` function
#'
#' @return A route satisfying the range constraint, represented as an integer vector
#' @export
#'
initial_route2 <- function(p_inst, L, info) {
  # For testing purposes:
  # inst <- test_instances$p7_chao;L <- 100;variances <- generate_variances(inst);info <- generate_information(inst);p_inst <- prepare_instance(inst, variances, info)

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
  while(!route_concluded) {
    # Decide on the next node
    sdr <- get_SDR(current_node, L_remaining, score)
    candidates <- sdr |> names() |> as.integer()

    if (length(candidates) > 1) { # there are multiple candidates
      node_id <- sample(candidates, 1, prob = sdr)
    } else if (length(candidates) == 1) { # there is only one candidate
      node_id <- candidates[1]
    } else if (length(candidates) < 1) { # there are no feasible candidates
      node_id <- 1; route_concluded <- T
    }

    # Find path to next and append to route
    path_to_next <- sp(current_node, node_id)
    route <- append(route, path_to_next)

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
  }
  message("Total realized score is: ", round(sum(p_inst$points$realized_score[route]), 1))
  message("Total expected score is: ", sum(p_inst$points$score[route]))
  message("L is: ", round(L - L_remaining,1))
  # message("Route is: ", route)
  # return(route)
  structure(
    list(
      "route" = route,
      "realized_score" = sum(p_inst$points$realized_score[route]),
      "expected_score" = sum(p_inst$points$score[route]),
      "L" = L - L_remaining
    ),
    class = "initial_route"
  )
}
