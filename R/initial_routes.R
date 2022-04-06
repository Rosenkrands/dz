#' Generation of inital routes as part of the pre-mission clustering
#'
#' @param inst Instance object returned from the `instance` function
#' @param L The range for each agent (homogenous agents are assumed)
#' @param variances A set of variances returned from the `generate_variances` function
#' @param info An information matrix returned by the `generate_information` function
#'
#' @return A route satisfying the range constraint, represented as an integer vector
#' @export
#'
initial_route <- function(inst, L, variances, info) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)
  L_remaining <- L

  inst$points <- inst$points |>
    dplyr::left_join(variances, by = c("id")) |>
    dplyr::rowwise() |>
    dplyr::mutate(realized_score = ifelse(is.na(score_variance), 0, rnorm(1, mean = score, sd = sqrt(score_variance)))) |>
    dplyr::mutate(unexpected = (realized_score > (score+1.96*sqrt(score_variance))) | (realized_score < (score-1.96*sqrt(score_variance))),
                  unexpected = tidyr::replace_na(unexpected, F))

  score <- inst$points$score
  realized_score <- inst$points$realized_score
  unexpected <- inst$points$unexpected

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
  first_node <- sample(
    inst$points$id[inst$dst[1,] < L/2], # consider only points that can be reached
    size = 1,
    prob = sdr[inst$dst[1,] < L/2]
  )

  # find the shortest path to the next node and append to route
  path_to_next <- sp(1, first_node)
  route <- append(route, path_to_next)

  # collect profits and update the remaining range
  s_total <- s_total + sum(inst$points$realized_score[path_to_next])
  inst$points$realized_score[path_to_next] <- 0
  inst$points$score[path_to_next] <- 0
  L_remaining <- L_remaining - dst[1, first_node]

  # add points until there is no more range
  current_node <- route[length(route)]
  while(current_node != 1) {
    # invisible(readline(prompt = "press [enter] to continue"))
    # calculate the SDR for all points and select the best
    sdr <- tidyr::replace_na(inst$points$score / inst$dst[current_node,], 0) |>
      sort(decreasing = T)

    # check if there is enough range to visit the point and return to depot
    for (i in 1:length(sdr)) {

      node_id <- as.integer(names(sdr[i]))

      # skip if node id is 1 or current node
      if (node_id %in% c(1,current_node)) next

      L_cost <- dst[current_node, node_id] + dst[node_id, 1]

      if (L_cost <= L_remaining) {
        # The remaining range allows, so we can add the next node
        path_to_next <- sp(current_node, node_id)
        route <- append(route, path_to_next)

        # collect profits and update the remaining range
        s_total <- s_total + sum(inst$points$realized_score[path_to_next])

        # check if the score was unexpected and update the correlated scores
        for (j in path_to_next) { # we need to consider all nodes in the shortest path
          if (inst$points$unexpected[j]) {
            related_nodes <- which(info[j,] != 0) # find the nodes that are related
            for (k in related_nodes) { # update both score and realized scores for all related nodes
              inst$points$realized_score[k] <- inst$points$realized_score[k] + info[j,k] * inst$points$realized_score[j]
              inst$points$score[k] <- inst$points$score[k] + info[j,k]*inst$points$score[j]
            }
            inst$points$unexpected[j] <- F
          }
        }

        inst$points$realized_score[path_to_next] <- 0
        inst$points$score[path_to_next] <- 0
        L_remaining <- L_remaining - dst[current_node, node_id]
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
      L_remaining <- L_remaining - dst[current_node, 1]
    }
    current_node <- route[length(route)]
    # print(route)
  }
  message("Total realized score is: ", round(s_total, 2))
  message("Total expected score is: ", sum(score[unique(route)]))
  message("L is: ", L - L_remaining)
  # message("Route is: ", route)
  # return(route)
  structure(
    list(
      "route" = route,
      "realized_score" = s_total,
      "expected_score" = sum(score[unique(route)]),
      "L" = L - L_remaining
    ),
    class = "initial_route"
  )
}

#' Plot the initial route on the instance
#'
#' @param init_route A list returned by the `initial_route` function
#' @param inst
#'
#' @return
#' @export
#'
#' @examples
plot.initial_route <- function(init_route, inst) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)

  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = init_route$route) |>
    dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-route) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end")) |>
    dplyr::group_by(x,y,xend,yend)

  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "intermediate"),
      # ggplot2::aes(x, y, size = score, color = score, shape = point_type)
      ggplot2::aes(x, y, shape = point_type)
    ) +
    ggplot2::geom_segment(
      data = inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none")
}

# # set.seed(6) # 40 - 40
# inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)
# (temp_route <- initial_route(inst, L, variances, info))
#
# plot_initial_route <- function(temp_route) {
#   route_segments <- tibble::tibble(route = temp_route$route) |>
#     dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
#     dplyr::filter(!is.na(id_start)) |>
#     dplyr::select(-route) |>
#     dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
#                       by = c("id_start" = "id")) |>
#     dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
#                       by = c("id_end" = "id"), suffix = c("","end")) |>
#     dplyr::group_by(x,y,xend,yend)
#
#   ggplot2::ggplot() +
#     ggplot2::geom_point(
#       data = inst$points |> dplyr::filter(point_type == "intermediate"),
#       # ggplot2::aes(x, y, size = score, color = score, shape = point_type)
#       ggplot2::aes(x, y, shape = point_type)
#     ) +
#     ggplot2::geom_segment(
#       data = inst$edges,
#       ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
#       color = ggplot2::alpha("black", 0.3), linetype = "dashed"
#     ) +
#     ggplot2::geom_segment(
#       data = route_segments,
#       ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
#     ) +
#     ggplot2::geom_point(
#       data = inst$points |> dplyr::filter(point_type == "terminal"),
#       ggplot2::aes(x, y), color = "red", shape = 17
#     ) +
#     ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
#     ggplot2::theme_bw() +
#     ggplot2::guides(shape = "none", size = "none")
# }
#
# plot_initial_route(initial_route(inst, L, variances, info))

#
# i = 1234
# while(T) {
#   cat(i, "\r")
#   set.seed(i) # 40 - 40
#   inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)
#   route <- initial_route(inst, L, variances, info)
#   # if (c(75, 101, 75, 101) %in% route) break
#   x <- c(75,100,75,100)
#   if (any(apply(embed(route,length(route)-length(x)+1),2,identical,x))) break
#   i <- i + 1
# }
