library(dz)
set.seed(7)

# Setting parameters
inst <- test_instances$p7_chao
L <- 100
variances <- generate_variances(inst)
info <- generate_information(inst)

# Prepare instance
p_inst <- prepare_instance(inst, variances, info)
# (p <- plot(p_inst))

route1 <- initial_route2(p_inst, L = 100, info)
route2 <- initial_route2(p_inst, L = 100, info)

# Generate route segments based on the route
route_segments <- tibble::tibble(route = c(route1$route, route2$route), route_id = c(rep(1, length(route1$route)), rep(2, length(route2$route)))) |>
  dplyr::group_by(route_id) |>
  dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-route) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end")) |>
  dplyr::group_by(x,y,xend,yend)

p <- ggplot2::ggplot() +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "intermediate"),
    # ggplot2::aes(x, y, size = score, color = score, shape = point_type)
    ggplot2::aes(x, y, shape = point_type)
  ) +
  ggplot2::geom_segment(
    data = p_inst$edges,
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    color = ggplot2::alpha("black", 0.3), linetype = "dashed"
  ) +
  ggplot2::geom_segment(
    data = route_segments |> dplyr::filter(route_id == 1),
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color=as.character(route_id)),
    position = ggplot2::position_nudge(x = .2, y = .2)
  ) +
  ggplot2::geom_segment(
    data = route_segments |> dplyr::filter(route_id == 2),
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color=as.character(route_id)),
    position = ggplot2::position_nudge(x = -.2, y = -.2)
  ) +
  ggplot2::geom_point(
    data = p_inst$points |> dplyr::filter(point_type == "terminal"),
    ggplot2::aes(x, y), color = "red", shape = 17
  ) +
  ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
  ggplot2::theme_bw() +
  ggplot2::guides(shape = "none", size = "none")

# calculate the position-based dissimilarity

routes <- list(route1$route, route2$route)

longest_route <- do.call(c, lapply(routes, length)) |> which.max()

distance_to_closest <- function(node_id, other_route) {
  # node_id <- routes[[longest_route]][2]; other_route <- routes[-longest_route][[1]]
  sub_dst <- p_inst$dst[node_id, unique(other_route)]

  closest_node_id <- sub_dst |> which.min() |> names() |> as.integer()
  distance <- sub_dst |> min()

  return(tibble::tibble(node_id, closest_node_id, distance))
}

rslt <- do.call(
  dplyr::bind_rows,
  lapply(
    routes[[longest_route]] |> as.list(),
    function(x) distance_to_closest(x, routes[-longest_route][[1]])
  )
)

arrow_length = 5
i = 1

plot_data <- rslt[i, ] |>
  dplyr::left_join(
    p_inst$points |> dplyr::select(id,x,y),
    by = c("node_id" = "id")
  ) |>
  dplyr::left_join(
    p_inst$points |> dplyr::select(id,x,y),
    by = c("closest_node_id" = "id"),
    suffix = c("", "_closest")
  ) |>
  dplyr::mutate(xend = x + arrow_length, yend = y + arrow_length,
                x_closest_end = x_closest - arrow_length, y_closest_end = y_closest + arrow_length)

print(p +
  ggplot2::geom_segment(
    data = plot_data,
    ggplot2::aes(x=x,y=y,xend=xend,yend=yend),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), ends = "first")
  ) +
  ggplot2::geom_segment(
    data = plot_data,
    ggplot2::aes(x=x_closest,y=y_closest,xend=x_closest_end,yend=y_closest_end),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.03, "npc"), ends = "first")
  ))
i = i + 1

