#' Greedy randomized route generation
#'
#' @param p_inst prepared instance returned from `prepare_instance`
#' @param zone_id zone_id for use after zoning, default to 0 which uses all nodes
#' @param L length constraint for the route, if L < shortest path from source to sink the shortest path is returned
#' @param info information object returned from `generate_information`
#' @param top_percentile the top percentile of candidate nodes to use, 0 is only the best and 1 is completely random
#' @param nghbr_order order neighborhood of the current node to search for candidates in
#'
#' @return a greedy_route object
#' @export
#'
greedy_route <- function(p_inst, zone_id = 0, L, info, top_percentile = .5, nghbr_order = 1) {
  # Make copies of variables to alter during route generation
  score <- p_inst$points$score
  expected_score <- p_inst$points$expected_score
  realized_score <- p_inst$points$realized_score

  # reuse igraph created during clustering
  if (zone_id == 0) {
    g <- inst$g
    dst <- inst$dst
  } else {
    stop("Not implemented for zone_id != 0")
  }

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

  get_SDR <- function(current_node, L_remaining, score, top_percentile) {
    # current_node = 1

    # neighborhood of current node
    nghbr <- igraph::neighborhood(
      g, order = nghbr_order, nodes = as.character(current_node)
    )[[1]] |> names()

    # The shortest paths to all node
    paths <- igraph::shortest_paths(
      g,
      from = as.character(current_node),
      to = nghbr
    )$vpath

    # The gathered profit from a path
    s <- do.call(
      c,
      lapply(
        paths,
        function(x) score[x |> names() |> as.integer()] |> sum()
      )
    )

    if (all(s == 0)) warning("All neighbors have already been visited")

    # The distance of a path
    d <- dst[current_node, nghbr]

    # can we get to a node and back to source
    feasible <- d + dst[nghbr, nrow(inst$points)] <= L_remaining

    # set the infeasible nodes to 0 including the current node
    r <- s/d * feasible; r[is.na(r) | !is.finite(r)] <- 0

    # return SDR for the feasible nodes
    candidates <- r[(r > 0) & !(names(r) %in% c("1", as.character(nrow(p_inst$points))))]
    rslt <- sort(candidates, decreasing = T)[1:round(length(candidates)*top_percentile)]
    if (any(is.na(rslt))) return(integer()) else return(rslt)
  }

  while(!route_concluded) {
    # Decide on the next node
    sdr <- get_SDR(current_node, L_remaining, expected_score, top_percentile)
    candidates <- sdr |> names() |> as.integer()

    if (length(candidates) > 1) { # there are multiple candidates
      node_id <- sample(candidates, 1, prob = sdr)
    } else if (length(candidates) == 1) { # there is only one candidate
      node_id <- candidates[1]
    } else if ((length(candidates) < 1)) { # there are no feasible candidates
      node_id <- nrow(p_inst$points); route_concluded <- T
    }

    # print(node_id)

    # Find path to next and append to route
    path_to_next <- sp(current_node, node_id)
    route <- append(route, path_to_next)

    # Update variables
    expected_score[path_to_next] <- 0
    L_remaining <- L_remaining - dst[current_node, node_id]

    current_node <- tail(route, 1)
    # print(route)
  }

  structure(
    list(
      "route" = route,
      "realized_score" = sum(p_inst$points$realized_score[route]),
      "expected_score" = sum(p_inst$points$expected_score[route]),
      "L" = L - L_remaining,
      "p_inst" = p_inst
    ),
    class = "greedy_route"
  )
}

#' Plot method for greedy_route
#'
#' @param gr a greedy_route object returned from the `greedy_route` function
#' @param label whether to show "points" or "text" (node ids)
#'
#' @return a ggplot object
#' @export
#'
plot.greedy_route <- function(gr, label = "point") {
  # For testing purposes:

  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = gr$route) |>
    dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-route) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end")) |>
    dplyr::group_by(x,y,xend,yend)

  p <- ggplot2::ggplot()

  if (label == "point") {
    p <- p +
      ggplot2::geom_point(
        data = gr$p_inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, size = score, color = score, shape = point_type), alpha = .7
      )
  } else if (label == "text") {
    p <- p +
      ggplot2::geom_text(
        data = gr$p_inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, label = id)
      )
  }

  p <- p +
    ggplot2::geom_segment(
      data = gr$p_inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend), color = "red"
    ) +
    ggplot2::geom_point(
      data = gr$p_inst$points |> dplyr::filter(point_type %in% c("source", "sink")),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::ggtitle(paste0("Instance: ", gr$p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::coord_fixed()

  return(p)
}
