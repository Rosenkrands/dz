# library(dz)
library(tidyverse)
set.seed(6)

inst = test_instances$p7_chao
L = 100
k = 4
variances = generate_variances(inst = inst)
info = generate_information(inst, r = 20)
p_inst = prepare_instance(inst, variances, info)
num_routes <- 100

init_routes <- 1:num_routes |> as.list() |> pbapply::pblapply(function(x) {initial_route2(p_inst, L, info, top_percentile = .5)})

# Generate route segments based on the route
route_segments <- tibble::tibble(route = lapply(init_routes, function(x) x$route), route_id = 1:num_routes) |>
  unnest(cols = route) |>
  group_by(route_id) |>
  dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-route) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end")) |>
  dplyr::group_by(route_id) |>
  dplyr::mutate(n = rnorm(1)/2, across(x:yend, ~ .x + n))

ggplot2::ggplot() +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "intermediate"),
    ggplot2::aes(x, y, size = score, shape = point_type), alpha = .2
    # ggplot2::aes(x, y, shape = point_type)
  ) +
  ggplot2::geom_segment(
    data = p_inst$edges,
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    color = ggplot2::alpha("black", 0.3), linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend),#, color = as.character(route_id))
    alpha = .5
  ) +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "terminal"),
    ggplot2::aes(x, y), color = "red", shape = 17
  ) +
  # ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
  ggplot2::theme_bw() +
  ggplot2::guides(shape = "none", size = "none", color = "none") +
  ggplot2::coord_fixed()

ggsave("./figures_for_presentation/constructed_routes.png", width = 5, height = 5)

rc <- route_clustering(p_inst, init_routes, 4, measure = "position-based", weigthed = F); cluster <- rc$cutree

route_segments <- route_segments |>
  left_join(tibble(route_id = 1:num_routes, cluster_id = cluster))

ggplot2::ggplot() +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "intermediate"),
    ggplot2::aes(x, y, size = score, shape = point_type), alpha = .2
    # ggplot2::aes(x, y, shape = point_type)
  ) +
  ggplot2::geom_segment(
    data = p_inst$edges,
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    color = ggplot2::alpha("black", 0.3), linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color = as.character(cluster_id)),
    alpha = .5
  ) +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "terminal"),
    ggplot2::aes(x, y), color = "red", shape = 17
  ) +
  # ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
  ggplot2::theme_bw() +
  ggplot2::guides(shape = "none", size = "none", color = "none") +
  ggplot2::coord_fixed()

ggsave("./figures_for_presentation/constructed_routes_w_zones.png", width = 5, height = 5)

library(dendextend)
dend <- rc$hclust %>% as.dendrogram %>%
  set("branches_k_color", k=4)
# plot(
#   dend,
#   leaflab = "none"
# )
ggplot(as.ggdend(dend), labels = F) + theme_bw() + labs(x = "", y = "")
ggsave("./figures_for_presentation/dendogram.png", width = 5, height = 5)

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

  # print(plot_zones(zones))
}

plot_zones_disputed <- function(zones) {
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
    dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0) | (disputed.x == 1) | (disputed.y == 1)) |>
    # dplyr::filter((zone.x == zone.y) | (zone.x == 0) | (zone.y == 0)) |>
    dplyr::filter(!is.na(zone.x) & !is.na(zone.y)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y)) |>
    dplyr::distinct(ind1, ind2, .keep_all = T)

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
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::filter(!is.na(disputed), disputed == 0),
        # dplyr::filter(!is.na(disputed)),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score)
    ) +
    ggplot2::geom_point(
      data = p_inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::filter(!is.na(disputed), disputed == 1),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score), shape = 1
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    # ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
    ) +
    labs(x = "x", y = "y")
}

zones[[2]] <- zones[[2]][zones[[2]] != 85]
zones[[1]] <- append(zones[[1]], 85)

plot_zones_disputed(zones) + ggplot2::coord_fixed()
ggsave("./figures_for_presentation/overlapping_zones.png", width = 5, height = 5)

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
    # dplyr::filter(!is.na(zone.x) & !is.na(zone.y)) |>
    dplyr::mutate(zone = ifelse(zone.x == 0, zone.y, zone.x)) |>
    dplyr::select(-c(zone.x,zone.y))
    # dplyr::distinct(ind1, ind2, .keep_all = T)

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
    # ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
    ) +
    labs(x = "x", y = "y")
}

plot_zones(zones) + ggplot2::coord_fixed()
ggsave("./figures_for_presentation/most_frequent_zones.png", width = 5, height = 5)


