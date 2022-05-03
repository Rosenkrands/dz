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
#' @param p_inst
#' @param L
#' @param k
#' @param info
#' @param dispute_obj
#' @param measure
#' @param weighted
#' @param shiny
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
