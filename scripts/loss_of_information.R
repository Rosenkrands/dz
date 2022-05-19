library(dz)
library(shiny)
set.seed(1)

# filter out the the terminal point, as we will use
# node 1 as both source and sink.
inst <- test_instances$p7_chao
inst$points <- inst$points |> dplyr::filter(id != nrow(inst$points))
plot(inst)

# find shortest path distance matrix
tri <- (deldir::deldir(inst$points$x, inst$points$y))$delsgs
tri$dist <- sqrt((tri$x1 - tri$x2)^2 + (tri$y1 - tri$y2)^2)

g <- igraph::graph_from_data_frame(
  tri |> dplyr::select(ind1, ind2, weight = dist),
  directed = FALSE,
  vertices = inst$points |> dplyr::select(id, score)
)

dst <- igraph::distances(g, algorithm = "dijkstra")

# generate information matrix
eps <- matrix(
  data = runif(nrow(inst$points)^2, min = -1, max = 1),
  nrow = nrow(inst$points)
)

info <- (eps / dst)
info[!is.finite(info)] <- 0
info[dst > 20] <- 0

# visualize information
plot_info <- function(node_id = 1, delaunay = T) {
  # Instantiate the ggplot object
  p <- ggplot2::ggplot()

  # Add delaunay edges
  tri <- deldir::deldir(inst$points$x, inst$points$y)
  p <- p +
    ggplot2::geom_segment(
      data = tri$delsgs,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    )

  affected <- which(info[node_id, ] != 0)

  # Add points and title to the plot
  p +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(!id %in% affected),# |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, shape = point_type), color = "darkgrey"
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(id %in% affected),# |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, size = abs(info[node_id,][affected]), color = as.character(sign(info[node_id, ][affected])), shape = point_type, alpha = .3)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(id == node_id),
      ggplot2::aes(x, y), color = "blue"
    ) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none") +
    ggplot2::scale_color_manual(values = c("red", "green")) +
    # ggplot2::scale_size(limits = c(0,1)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "")
}

zones <- list(
  c(1, 3, 5, 10, 17, 21, 27, 28, 29, 31, 40, 41, 47, 49, 50, 76, 84, 91, 92, 94, 51, 52, 16, 53, 42, 72, 70, 83, 71, 77),
  c(1, 2, 9, 12, 13, 14, 15, 22, 23, 24, 37, 43, 57, 58, 59, 60, 61, 63, 64, 68, 75, 78, 85, 95, 98, 99, 100, 101, 36, 79, 67),
  c(1, 4, 6, 7, 8, 11, 18, 19, 20, 25, 26, 30, 32, 33, 34, 35, 39, 44, 46, 48, 54, 62, 65, 69, 81, 82, 87, 89, 90, 93, 96, 97, 66,55,38, 56, 86, 80, 74, 88, 45, 73)
)

plot_info_zones <- function(node_id = 1, delaunay = T, zones) {
  # Instantiate the ggplot object
  p <- ggplot2::ggplot()

  # Add same zone delaunay edges
  temp <- tibble::tibble(id = zones, agent_id = 1:length(zones)) |>
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

  p <- p +
    ggplot2::geom_segment(
      data = same_zone_edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    )

  affected <- which(info[node_id, ] != 0)

  # Add points and title to the plot
  p +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(temp, by = c("id")) |>
        dplyr::filter(!id %in% affected),# |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, shape = point_type), color = "darkgrey"
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(id %in% affected),# |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, size = abs(info[node_id,][affected]), color = as.character(sign(info[node_id, ][affected])), shape = point_type, alpha = .3)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(id == node_id),
      ggplot2::aes(x, y), color = "blue"
    ) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none") +
    ggplot2::scale_color_manual(values = c("red", "green")) +
    # ggplot2::scale_size(limits = c(0,1)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "", y = "")
}

plot_info_zones(node_id = 2, delaunay = T, zones)
ggplot2::ggsave("C:/users/krose/Desktop/information_2.pdf", width = 4, height = 4)

plot_info(node_id = 2)
ggplot2::ggsave("C:/users/krose/Desktop/information_1.pdf", width = 4, height = 4)

# plot_info(node_id = 50)
# ggplot2::ggsave("C:/users/krose/Desktop/information_2.pdf", width = 3, height = 3)

