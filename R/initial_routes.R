#' Generation of inital routes as part of the pre-mission clustering
#'
#' This function will sample the first node between all nodes in proportion to SDR, afterwards it will select the next node greedily maximizing SDR.
#' It will update expected score based on whether anything unexpected comes up.
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
  # TODO: adapt to use the prepared instance
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)
  L_remaining <- L

  # TODO: make a function to do this
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
              # Realized score should already be updated beforehand
              # inst$points$realized_score[k] <- inst$points$realized_score[k] + info[j,k]
              inst$points$score[k] <- inst$points$score[k] + info[j,k]
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

#' Perform hierarchical clustering based on initial routes
#'
#' @param init_routes a list of initial routes obtained from the `initial_route` function
#' @param k the number of agents
#' @param measure what dissimilarity measure to use, can be either "element-based" or "position-based". Defaults to "position-based".
#'
#' @return a list containing the `hclust` object and a vector of the clusters
#' @export
#'
route_clustering <- function(p_inst, init_routes, k, measure = "position-based", weigthed = T) {
  if (measure == "element-based") {
    compute_dissimilarity <- function(i,j) {
      nodes_i <- unique(init_routes[[i]]$route)
      nodes_j <- unique(init_routes[[j]]$route)
      difference <- setdiff(nodes_i, nodes_j)
      return(length(difference))
    }

    n <- length(init_routes)
    dissimilarity <- matrix(nrow = n, ncol = n)
    for (i in 1:n) {
      for (j in 1:n) {
        dissimilarity[i,j] <- compute_dissimilarity(i,j)
      }
    }
  } else if (measure == "position-based") {
    position_dissimilarity <- function(ids) {
      # Determine the longest route
      routes <- list(init_routes[[ids[1]]]$route, init_routes[[ids[2]]]$route)
      longest_route <- do.call(c, lapply(routes, length)) |> which.max()

      mean_dist <- sapply(
        routes[[longest_route]],
        function(node_id) {
          p_inst$dst[node_id, unique(routes[-longest_route][[1]])] |> min()
        }
      ) |> mean()

      # beta determines if the realized score of the two routes are close to each other
      beta <- 1/abs((init_routes[[ids[1]]]$realized_score - init_routes[[ids[2]]]$realized_score))

      return(c("mean_dist" = mean_dist, "beta" = beta))
    }

    # find all combinations of routes and compute dissimilarity
    combinations <- utils::combn(1:length(init_routes), 2, simplify = F)
    dis <- pbapply::pblapply(combinations, position_dissimilarity)

    # create matrix to hold results
    dissimilarity <- matrix(data = 0, nrow = length(init_routes), ncol = length(init_routes))

    # post processing to weight dissimilarities

    # mu is the mean realized score for the initial routes
    mu <- mean(sapply(init_routes, function(x) x$realized_score))
    mu_pbd <- mean(sapply(dis, function(x) x["mean_dist"]))
    all_beta <- sapply(dis, function(x) x["beta"])
    max_beta <- max(all_beta[!is.na(all_beta) & is.finite(all_beta)])

    adjustment <- numeric(length = length(combinations))

    # take dissimilarity values from the list and insert into the matrix
    lapply(seq_along(combinations), function(i) {
      ids <- combinations[[i]]

      # alpha determines the ratio of ids[1]'s realized to the mean realized score
      alpha <- init_routes[[ids[1]]]$realized_score/mu

      if (!is.finite(all_beta[i])) beta <- max_beta else beta <- all_beta[i]

      gamma <- dis[[i]]["mean_dist"] - mu_pbd

      adjustment[i] <<- alpha*beta*gamma

      if (weigthed) {
        dissimilarity[ids[1], ids[2]] <<- dis[[i]]["mean_dist"] * alpha * beta * gamma
      } else {
        dissimilarity[ids[1], ids[2]] <<- dis[[i]]["mean_dist"]
      }
    })

    # mirror the upper part of the matrix into the lower part
    dissimilarity <- dissimilarity + t(dissimilarity)
  }

  hc <- stats::hclust(as.dist(dissimilarity))
  cluster <- cutree(hc, k)
  return(
    list("hclust" = hc, "cutree" = cluster)
  )
}

#' Resolve disputes so each node are only assigned one cluster
#'
#' @param init_routes a list of initial routes obtained from the `initial_route` function
#' @param cluster a vector of clusters obtained from the `route_clustering` function
#' @param obj what objective to use when solving the disputes, currently there are "most_frequent" and "highest_score"
#'
#' @return list containing the zones as well as the zones of only undisputed nodes
#' @export
#'
resolve_disputes <- function(init_routes, cluster, obj = "most_frequent") {
  k <- length(unique(cluster))

  route_info <- tibble::tibble(
    id = lapply(init_routes, function(x) x$route),
    realized_score = do.call(c, lapply(init_routes, function(x) x$realized_score)),
    expected_score = do.call(c, lapply(init_routes, function(x) x$expected_score)),
    cluster,
    route_id = 1:length(init_routes)
  )

  route_count <- route_info |>
    dplyr::group_by(cluster) |>
    dplyr::summarise(n_route = dplyr::n_distinct(route_id))

  route_score <- route_info |>
    tidyr::unnest(cols = id) |>
    dplyr::distinct() |> # only one node id per route
    dplyr::group_by(id, cluster) |>
    dplyr::summarise(mean_expected_score = mean(expected_score),
                     mean_realized_score = mean(realized_score),
                     n_route_id = dplyr::n_distinct(route_id))

  node_usage <- route_info |>
    tidyr::unnest(cols = id) |>
    dplyr::group_by(id, cluster) |>
    dplyr::summarise(n = dplyr::n()) |> # number of times the node id is used in the cluster
    dplyr::left_join(route_count, by = c("cluster")) |>
    dplyr::mutate(n = n / n_route) |> # usage adjusted for number of routes
    dplyr::left_join(route_score, by = c("id", "cluster")) |>
    dplyr::summarise(num_cluster_use = dplyr::n_distinct(cluster),
                     most_frequent = dplyr::first(cluster, order_by = -n),
                     highest_mean_realized_score = dplyr::first(cluster, order_by = -mean_realized_score)) |>
    dplyr::mutate(disputed = ifelse(num_cluster_use > 1, 1, 0)) |>
    dplyr::ungroup()

  # Construct the zones based the node_usage tibble
  zones_undisputed <- list() # kept for illustrative purposes
  zones <- list()

  for (i in 1:k) {
    # add the most frequent undisputed points to each zone
    filtered_nodes <- node_usage |>
      purrr::when(obj == "most_frequent" ~ dplyr::filter(., most_frequent == i),
                  obj == "highest_score" ~ dplyr::filter(., highest_mean_realized_score == i),
                  ~ stop(paste0("obj: ", obj, " not recognized in resolve_disputes()")))

    ids_undisputed <- filtered_nodes |>
      dplyr::filter(disputed == 0) |>
      dplyr::pull(id)

    ids <- filtered_nodes |>
      dplyr::pull(id)

    zones_undisputed[[i]] <- unique(c(1, ids_undisputed))
    zones[[i]] <- unique(c(1, ids))
  }

  return(
    list("zones" = zones, "zones_undisputed" = zones_undisputed)
  )
}

#' Fix connectivity in zones
#'
#' @param zones
#'
#' @return
#' @export
#'
fix_connectivity <- function(inst, zones, available_nodes = integer()) {
  k <- length(zones)

  # function to determine if a zone is connected
  connected <- function(zone) {
    # zone <- zones[[1]]
    sub_g <- igraph::induced_subgraph(inst$g, vids = zone)
    igraph::is_connected(sub_g, mode = "weak") # check for undirected path between pairs of vertices
  }

  # Check the connectedness of each zone
  are_connected <- do.call(c, lapply(zones, connected))

  if (!all(are_connected)) {
    # de-zone points that are not connected to the source through its own zone
    # available_nodes <- integer()
    if (length(available_nodes) > 0) stop("not all zones are connected but available nodes was provided. Please fix the connectivity issue before providing available nodes.")

    for (i in 1:k) {
      # Make the sub graph induced by each zone
      sub_g <- igraph::induced_subgraph(inst$g, vids = zones[[i]])
      if (!igraph::is_connected(sub_g, mode = "weak")) {
        # The problem is the nodes that are not connected to the source,
        # nodes that are not connected to the source will have distance == Inf
        temp_dst <- igraph::distances(sub_g, v = 1) # calculate distances
        unconnected <- igraph::V(sub_g)[temp_dst == Inf] |> # get the node id for nodes with distance == Inf
          names() |> as.integer()

        # Idea: discard the nodes from the zone and see if we can add them to another zone
        available_nodes <- append(available_nodes, unconnected)
        zones[[i]] <- zones[[i]][!zones[[i]] %in% unconnected]
      }
    }
  }

  # save number of nodes with connectivity issues
  num_issues <- length(available_nodes)

  # make function to evaluate each zone based on objective
  avg_dist <- function(zone) {
    # subset the distance matrix (of shortest paths) to only nodes in the zone
    dst_temp <- inst$dst[zone, zone]
    # return the mean distance between nodes in the zone
    mean(dst_temp[lower.tri(dst_temp, diag = F)])
  }
  # do.call(sum, lapply(zones, avg_dist))

  if (!all(are_connected) & (length(available_nodes) == 0)) {
    num_issues <- 0
    return(
      list("zones" = zones, "num_issues" = num_issues)
    )
  }

  while (length(available_nodes) > 0) {
    # Check the result of inserting an available node into each zone
    candidates <- list(); l = 0

    for (i in available_nodes) {
      for (j in 1:k) {
        # cat("i is:", i, "j is:", j, "\n")
        l = l + 1
        # add available node to the zone
        temp_zones <- zones
        temp_zones[[j]] <- append(temp_zones[[j]], i)

        # see if the zone is connected
        sub_g <- igraph::induced_subgraph(inst$g, vids = temp_zones[[j]])
        connected <- igraph::is_connected(sub_g, mode = c("weak"))

        # calculate the average distance
        if (connected) {
          zone_avg_dist <- avg_dist(temp_zones[[j]])
        } else {
          zone_avg_dist <- NA
        }

        candidates[[l]] <- tibble::tibble(
          available_node = i, zone = j, is_connected = connected, avg_dist = zone_avg_dist
        )
      }
    }

    best_candidate <- do.call(dplyr::bind_rows, candidates) |>
      dplyr::filter(is_connected == T) |>
      dplyr::filter(avg_dist == min(avg_dist))

    if (nrow(best_candidate) < 1) {
      warning("there are no best candidate")
      # TODO: handle no candidate is_connected
    }

    zones[[best_candidate$zone]] <- append(
      zones[[best_candidate$zone]], best_candidate$available_node
    )
    available_nodes <- available_nodes[available_nodes != best_candidate$available_node]
  }

  return(
    list("zones" = zones, "num_issues" = num_issues)
  )
}

#' Routing based clustering
#'
#' @param inst
#' @param L
#' @param k
#' @param variances
#' @param info
#'
#' @return
#' @export
#'
rb_clustering <- function(p_inst, L, k, num_routes, info, dispute_obj = "most_frequent", measure = "position-based", weigthed = T,  shiny = F) {
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); p_inst <- prepare_instance(inst, variances, info); dispute_obj = "most_frequent"; shiny = F; num_routes = 100; measure = "position-based"; weigthed = T; set.seed(3)

  # First we generate the initial routes
  message("Construct the initial routes")
  suppressMessages(
    init_routes <- 1:num_routes |> as.list() |> pbapply::pblapply(function(x) {if (shiny) shiny::incProgress(amount = 1/num_routes); initial_route2(p_inst, L, info)})
  )

  # Then we perform the route clustering
  message("Performing the clustering")
  rc <- route_clustering(p_inst, init_routes, k, measure = measure, weigthed = weigthed); cluster <- rc$cutree

  # Next is assignment of the disputed points
  suppressMessages(
    {rd <- resolve_disputes(init_routes, cluster, obj = dispute_obj); zones <- rd$zones}
  )

  # Lastly we fix any issues with connectivity
  message("Fixing any connectivity issues")
  fc <- fix_connectivity(inst, zones); zones <- fc$zones; num_issues <- fc$num_issues
  message(paste0("number of issues fixed: ", num_issues))

  # Check if zones are connected through the source
  spans_source <- function(zone) {
    sub_g <- igraph::induced_subgraph(inst$g, vids = zone[zone != 1])
    igraph::is_connected(sub_g, mode = "weak")
  }
  pass <- do.call(c, lapply(zones, spans_source))

  if (any(!pass)) {# handle the zone(s) that span the source
    warning("A zone spans across the source, trying to re-zone one half of the spanning zone")
    problems <- which(!pass)

    if (length(problems) > 1) stop("More than one zone spans the source")

    zone <- zones[[problems[1]]]

    # Find out how many subclusters there are
    sub_g <- igraph::induced_subgraph(inst$g, vids = zone[zone != 1])
    decomp <- igraph::clusters(sub_g)

    if (decomp$no != 2) stop("There are not two sub clusters in the zone that spans the source")

    # dezone the nodes from the smallest sub cluster
    available_nodes <- decomp$membership[
      decomp$membership == which.min(decomp$csize)
    ] |> names() |> as.integer()
    zones[[problems[1]]] <- zones[[problems[1]]][!zones[[problems[1]]] %in% available_nodes]

    message(paste0("number of nodes rezoned: ", min(decomp$csize)))

    fc <- fix_connectivity(inst, zones, available_nodes); zones <- fc$zones
  }

  structure(
    list(
      "zones" = zones,
      "num_issues" = num_issues,
      "p_inst" = p_inst,
      "info" = info,
      "spanning_source" = any(!pass)
    ),
    class = "rb_clustering"
  )
}

#' Plot the result from routing-based clustering
#'
#' @param rb_clust
#' @param inst
#'
#' @return
#' @export
#'
plot.rb_clustering <- function(rb_clust) {
  # inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); rb_clust <- rb_clustering(inst, L, k, num_routes = 100, info)
  p_inst <- rb_clust$p_inst

  temp <- tibble::tibble(id = rb_clust$zones, agent_id = 1:length(rb_clust$zones)) |>
    tidyr::unnest(cols = id)

  same_zone_edges <- p_inst$edges |>
    dplyr::left_join(
      temp |> dplyr::rename(zone = agent_id),
      by = c("ind1" = "id")
    ) |>
    dplyr::left_join(
      temp |> dplyr::rename(zone = agent_id),
      by = c("ind2" = "id")
    ) |>
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score, alpha = score_variance)
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
    )
}
