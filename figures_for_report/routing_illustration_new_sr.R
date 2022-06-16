library(dz)
library(tidyverse)

inst <- test_instances$p7_chao; k = 4; L = 100
variances <- generate_variances(inst)
info <- generate_information(inst, r = 20)
p_inst <- prepare_instance(inst, variances, info)

clust_ls <- clustering(
  inst = p_inst,
  k,
  L,
  eps = 0,
  variances,
  info,
  cluster_method = "local_search",
  alpha = 0
)

zones <- clust_ls$cl$zones

sr <- starting_routes(inst, zones, L, L_budget = c("initial" = .80, "improve" = .90))

plot_sr2 <- function(sr, inst, what_routes, initial = T) {
  temp <- tibble::tibble(id = sr$zones, agent_id = 1:length(sr$zones)) |>
    tidyr::unnest(cols = id)

  same_zone_edges <- inst$edges |>
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

  route_segments <- tibble::tibble(routes = what_routes, agent_id = 1:length(what_routes)) |>
    tidyr::unnest(routes) |>
    dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score)
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend, linetype = ifelse(initial, "Initial", "Improved"))
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      # color = "none",
      alpha = "none",
      size = "none"
    ) +
    ggplot2::labs(
      color = "Zone", x = "x", y = "y"
    ) +
    ggplot2::coord_fixed()
}

route_segments <- tibble::tibble(routes = sr$improved_routes, agent_id = 1:length(sr$improved_routes)) |>
  tidyr::unnest(routes) |>
  dplyr::group_by(agent_id) |>
  dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-routes) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end"))

plot_sr2(sr, inst, sr$initial_routes) +
  labs(linetype = "Route type") +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, linetype = "Improved"),
  ) +
  scale_linetype_manual(values = c("dashed", "solid"))
ggsave("./figures_for_report/example_of_sr.png", width = 5, height = 5)

ur <- update_routes2(p_inst, zones, L, k, sr, info)
route_segments <- tibble::tibble(routes = ur$routes, agent_id = 1:length(ur$routes)) |>
  tidyr::unnest(routes) |>
  dplyr::group_by(agent_id) |>
  dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-routes) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end"))

plot_sr <- function(sr, inst, what_routes) {
  temp <- tibble::tibble(id = sr$zones, agent_id = 1:length(sr$zones)) |>
    tidyr::unnest(cols = id)

  same_zone_edges <- inst$edges |>
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

  route_segments <- tibble::tibble(routes = what_routes, agent_id = 1:length(what_routes)) |>
    tidyr::unnest(routes) |>
    dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")),
      ggplot2::aes(x, y, color = as.character(agent_id), size = score)
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend, linetype = "Starting")
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      # color = "none",
      alpha = "none",
      size = "none"
    ) +
    ggplot2::labs(
      color = "Zone", x = "x", y = "y"
    ) +
    ggplot2::coord_fixed()
}
plot_sr(sr, inst, what_routes = sr$improved_routes) +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, linetype = "Updated"),
  ) +
  labs(linetype = "Route type")
ggsave("./figures_for_report/example_of_ur.png", width = 5, height = 5)
