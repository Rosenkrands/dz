pbapply::pboptions(use_lb = T)

inst <- test_instances$p10_christofides
plot(inst, label = "text")

plot(inst$g)

info <- generate_information(inst, r = 20)
p_inst <- prepare_instance(inst, info)
# plot(p_inst)

L = 100
k = 4

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, c('p_inst', 'inst', 'L'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

greedy_routes <- pbapply::pblapply(
  1:1000,
  function(x) greedy_route(p_inst, zone_id = 0, L = L, info, top_percentile = .5, nghbr_order = 2),
  cl = cl
)

parallel::stopCluster(cl)

init_routes <- greedy_routes

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

  return(c("mean_dist" = mean_dist))
}

# find all combinations of routes and compute dissimilarity
combinations <- utils::combn(1:length(init_routes), 2, simplify = F)

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, c("p_inst", 'init_routes'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

dis <- pbapply::pblapply(combinations, position_dissimilarity, cl = cl)

parallel::stopCluster(cl)

# create matrix to hold results
dissimilarity <- matrix(data = 0, nrow = length(init_routes), ncol = length(init_routes))

# take dissimilarity values from the list and insert into the matrix
pbapply::pblapply(seq_along(combinations), function(i) {
  ids <- combinations[[i]]
  dissimilarity[ids[1], ids[2]] <<- dis[[i]]["mean_dist"]
})

# mirror the upper part of the matrix into the lower part
dissimilarity <- dissimilarity + t(dissimilarity)

hc <- stats::hclust(as.dist(dissimilarity))
cluster <- cutree(hc, k)

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
  filtered_nodes <- node_usage |> dplyr::filter(most_frequent == i)

  ids_undisputed <- filtered_nodes |>
    dplyr::filter(disputed == 0) |>
    dplyr::pull(id)

  ids <- filtered_nodes |>
    dplyr::pull(id)

  zones_undisputed[[i]] <- unique(c(1, ids_undisputed))
  zones[[i]] <- unique(c(1, ids))
}

plot_zones <- function(zones) {
  undisputed <- tibble::tibble(id = lapply(zones_undisputed, function(x) x[-1]), disputed = 1) |>
    tidyr::unnest(cols = id)

  temp <- tibble::tibble(id = zones, agent_id = 1:length(zones)) |>
    tidyr::unnest(cols = id) |>
    dplyr::left_join(undisputed, by = "id") |>
    dplyr::mutate(disputed = abs(tidyr::replace_na(disputed, replace = 0) - 1))

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
      data = p_inst$points |> dplyr::filter(point_type %in% c("source", "sink")),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "node") |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::filter(is.na(disputed)),
      ggplot2::aes(x, y, size = score), color = "grey"
    ) +
    # ggplot2::geom_point(
    #   data = p_inst$points |>
    #     dplyr::filter(point_type == "intermediate") |>
    #     dplyr::left_join(temp, by = c("id")) |>
    #     dplyr::filter(!is.na(disputed), disputed == 1),
    #   ggplot2::aes(x, y, color = as.character(agent_id), size = score), shape = 1
    # ) +
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "node") |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::filter(!is.na(disputed)),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score)
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

plot_zones(zones)
