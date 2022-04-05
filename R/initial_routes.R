#' Generation of inital routes as part of the pre-mission clustering
#'
#' @param inst Instance object returned from the `instance` function
#' @param L The range for each agent (homogenous agents are assumed)
#' @param r The radius from a node that profits will be realized
#' @param variances A set of variances returned from the `generate_variances` function
#'
#' @return A route satisfying the range constraint, represented as an integer vector
#' @export
#'
initial_route <- function(inst, L, r, variances) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; r = 10; variances = generate_variances(inst = inst)
  inst$points <- inst$points |>
    dplyr::left_join(variances, by = c("id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(realized_score = ifelse(is.na(score_variance), 0, rnorm(1, mean = score, sd = sqrt(score_variance))))

  score <- inst$points$score
  realized_score <- inst$points$realized_score

  # reuse igraph created during clustering
  g <- inst$g
  dst <- inst$dst

  # Dist function that returns only the points in the path
  sp <- function(id1, id2){
    # handle identical ids
    if (id1 == id2) {
      warning("Trying to calculate the shortest path from one node to itself, returning 0")
      return(0)
    }

    # Find vertices that make up the path
    short_vert <- igraph::shortest_paths(graph = g, from = id1, to = id2, output = "vpath")$vpath[[1]] |>
      as.vector()

    # return the path not including the first point
    return(short_vert |> tail(-1))
  }

  # initalize route vector
  route <- c(1)
  s_total <- 0

  # select the first point to add
  sdr <- tidyr::replace_na(inst$points$score / inst$dst[1,], 0)
  first_node <- sample(1:inst$n, size = 1, prob = sdr)

  # find the shortest path to the next node and append to route
  path_to_next <- sp(1, first_node)
  route <- append(route, path_to_next)

  # collect profits and update the remaining range
  s_total <- s_total + sum(inst$points$realized_score[path_to_next])
  inst$points$realized_score[path_to_next] <- 0
  inst$points$score[path_to_next] <- 0
  L <- L - dst[1, first_node]

  # add points until there is no more range
  current_node <- route[length(route)]
  while(current_node != 1) {
    # update the score information in vicinity
    in_range <- ifelse(dst[current_node,] < r, 1, 0)

    scores_out <- as.numeric(inst$points$score) * !in_range # mean score for points that are not in range
    scores_in <- inst$points$realized_score * in_range # realized score for points that are in range

    # calculate the SDR for all points and select the best
    sdr <- tidyr::replace_na((scores_out + scores_in) / inst$dst[current_node,], 0) |>
      sort(decreasing = T)

    # check if there is enough range to visit the point and return to depot
    for (i in 1:length(sdr)) {
      # skip if node id is 1 or current node
      if (i %in% c(1,current_node)) next

      node_id <- as.integer(names(sdr[i]))
      L_cost <- dst[current_node, node_id] + dst[node_id, 1]

      if (L_cost <= L) {
        # The remaining range allows, so we can add the next node
        path_to_next <- sp(current_node, node_id)
        route <- append(route, path_to_next)

        # collect profits and update the remaining range
        s_total <- s_total + sum(inst$points$realized_score[path_to_next])
        inst$points$realized_score[path_to_next] <- 0
        inst$points$score[path_to_next] <- 0
        L <- L - dst[current_node, node_id]
        break
      }
    }

    if (i == length(sdr)) {
      # we have looked through all candidates, return to the depot
      # find the shortest path to the next node and append to route
      path_to_next <- sp(current_node, 1)
      route <- append(route, path_to_next)

      # collect profits and update the remaining range
      s_total <- s_total + sum(inst$points$realized_score[path_to_next])
      inst$points$realized_score[path_to_next] <- 0
      inst$points$score[path_to_next] <- 0
      L <- L - dst[1, first_node]
    }
    current_node <- route[length(route)]
  }
  return(route)
}
