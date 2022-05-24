library(dz)
set.seed(3)

inst <- test_instances$p7_chao
variances <- generate_variances(inst)
info <- generate_information(inst)

p_inst <- prepare_instance(inst, variances, info)

plot(initial_route2(p_inst, L = 100, info, top_percentile = .15), p_inst)

set.seed(40); inst = test_instances$p7_chao; L = 100; k = 3; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20); p_inst <- prepare_instance(inst, variances, info); dispute_obj = "most_frequent"; shiny = F; num_routes = 500; measure = "position-based"; weigthed = F; top_percentile = .5

# First we generate the initial routes
message("Construct the initial routes")
suppressMessages(
  init_routes <- 1:num_routes |> as.list() |> pbapply::pblapply(function(x) {if (shiny) shiny::incProgress(amount = 1/num_routes); initial_route2(p_inst, L, info, top_percentile = top_percentile)})
)

# animation::saveGIF(
#   lapply(seq_along(init_routes), function(i) {print(plot(init_routes[[i]], p_inst)); cat(i, '\r')}),
#   interval = .2, ani.width = 500, ani.height = 500
# )

# Then we perform the route clustering
message("Performing the clustering")
rc <- route_clustering(p_inst, init_routes, k, measure = measure, weigthed = F); cluster <- rc$cutree

# plot(rc$hclust)

# Next is assignment of the disputed points
# suppressMessages(
#   {rd <- resolve_disputes(init_routes, cluster, obj = dispute_obj); zones <- rd$zones}
# )

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

obj = "most_frequent"

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

  print(plot_zones(zones))
}

zones_undisputed

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
      data = p_inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "intermediate") |>
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
        dplyr::filter(point_type == "intermediate") |>
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

plot_zones(zones_undisputed)
