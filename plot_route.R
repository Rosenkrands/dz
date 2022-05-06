inst <- test_instances$p7_chao

plot_route <- function(init_route) {
  # For testing purposes:
  # inst = test_instances$p7_chao; L = 100; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)

  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = init_route) |>
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
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "")
}

plot_route(c(1,40,31,30,7,2,1))
ggsave("placeholder_for_name.pdf", width = 4, height = 4)
