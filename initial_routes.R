library(dz)
set.seed(1)
inst = test_instances$p7_chao; L = 100; r = 10; variances = generate_variances(inst = inst)

initial_route <- function(inst, L, r, variances) {
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

routes <- 1:100 |> as.list() |> pbapply::pblapply(function(x) {initial_route(inst, L, r, variances)})

compute_dissimilarity <- function(i,j) {
  nodes_i <- unique(routes[[i]])
  nodes_j <- unique(routes[[j]])
  difference <- setdiff(nodes_i, nodes_j)
  return(length(difference))
}

n <- length(routes)
dissimilarity <- matrix(nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    dissimilarity[i,j] <- compute_dissimilarity(i,j)
  }
}

hc <- stats::hclust(as.dist(dissimilarity))
plot(hc)

k = 4

cluster <- cutree(hc, k)

# for plotting
route_segments <- tibble::tibble(routes, cluster) |>
  tidyr::unnest(routes) |>
  dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-routes) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end")) |>
  dplyr::group_by(cluster,x,y,xend,yend) |>
  dplyr::summarise(n = dplyr::n())

node_usage <- tibble::tibble(id = routes, cluster) |>
  tidyr::unnest(id) |>
  dplyr::group_by(id, cluster) |>  # should maybe correct for number of routes
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::summarise(num_cluster_use = dplyr::n_distinct(cluster),
                   most_frequent = dplyr::first(cluster, order_by = -n)) |>
  dplyr::mutate(disputed = ifelse(num_cluster_use > 1, 1, 0)) |>
  dplyr::ungroup()

# Plot the segment on the existing plot
ggplot2::ggplot() +
  # ggplot2::geom_segment(
  #   data = inst$edges,
  #   ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
  #   color = ggplot2::alpha("black", 0.3), linetype = "dashed"
  # ) +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color = as.character(cluster), alpha = n),
  ) +
  ggplot2::geom_point(
    data = inst$points |> dplyr::filter(point_type == "terminal"),
    ggplot2::aes(x, y), color = "red", shape = 17
  ) +
  ggplot2::geom_point(
    data = inst$points |>
      dplyr::filter(point_type == "intermediate") |>
      dplyr::inner_join(node_usage, by = c("id")),
    ggplot2::aes(x, y, shape = as.character(disputed))
  ) +
  ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
  ggplot2::theme_bw() +
  ggplot2::guides(
    shape = "none",
    fill = "none",
    color = "none"
  )

# Assign disputed points to clusters
# First we find points that are only used by one cluster

zones <- list()

for (i in 1:k) {
  zones[[i]] <- integer()

  ids <- node_usage |>
    dplyr::filter(most_frequent == i, disputed == 0) |>
    dplyr::pull(id)

  zones[[i]] <- append(zones[[i]], ids)
}

# function to plot zones list
plot_zones <- function() {
  temp <- tibble::tibble(id = zones, agent_id = 1:k) |>
    tidyr::unnest(cols = id)

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::inner_join(node_usage, by = c("id")) |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::mutate(agent_id = tidyr::replace_na(agent_id, 0)),
      ggplot2::aes(x, y, shape = as.character(disputed), color = as.character(agent_id))
    ) +
    ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none"
    )
}
