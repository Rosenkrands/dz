library(dz)
set.seed(1)

# input parameters
inst <- test_instances$p7_chao
k <- 3
L <- 50

variances <- generate_variances(inst)
info <- generate_information(inst, r = 20)

# clustering
rb_clust <- rb_clustering(
  inst,
  L,
  k,
  num_routes = 100,
  variances,
  info,
  dispute_obj = "most_frequent"
)
plot(rb_clust, inst)

# Time to visit all nodes
rb_clust$zones

zone <- 2

sub_g <- igraph::induced_subgraph(inst$g, vids = rb_clust$zones[[zone]])
sub_dst <- igraph::distances(sub_g)

nodes <- rb_clust$zones[[zone]][-1]

tour_eval <- function(path) {
  # path = 1:length(nodes)
  tour <- c(1, path, 1)
  route <- stats::embed(tour, 2)[, 2:1]
  -sum(sub_dst[route])

  # tour <- c(1, nodes[path], 1)
  # tibble::tibble(id1 = tour, id2 = tour) |>
  #   dplyr::mutate(id1 = dplyr::lag(id1)) |>
  #   dplyr::filter(!is.na(id1) & !is.na(id2)) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(cost = sub_dst[as.character(id1), as.character(id2)]) |>
  #   dplyr::ungroup() |>
  #   dplyr::summarise(cost = -sum(cost)) |>
  #   dplyr::pull(cost)
}

test_ga <- GA::ga(
  type = "permutation",
  fitness = tour_eval,
  lower = 1,
  upper = length(nodes),
  maxiter = 100,
  run = 500,
  parallel = F
)

# Post process solution to get the corrected path using the shortest path functionality

sol <- c(1,nodes[summary(test_ga)$solution[2,]],1)

sp <- function(id1, id2){
  # handle identical ids
  if (id1 == id2) {
    warning("Trying to calculate the shortest path from one node to itself, returning 0")
    return(0)
  }

  # Find vertices that make up the path
  short_vert <- igraph::shortest_paths(graph = sub_g, from = id1, to = id2, output = "vpath")$vpath[[1]] |>
    names() |> as.integer()

  # return the path not including the first point
  return(short_vert |> tail(-1))
}

actual_path <- c(1)

for (i in 1:(length(sol) - 1)) {
  actual_path <- append(actual_path, sp(as.character(sol[i]), as.character(sol[i+1])))
}

# Generate route segments based on the route
route_segments <- tibble::tibble(route = actual_path) |>
  dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
  dplyr::filter(!is.na(id_start)) |>
  dplyr::select(-route) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_start" = "id")) |>
  dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                    by = c("id_end" = "id"), suffix = c("","end")) |>
  dplyr::group_by(x,y,xend,yend)

temp <- tibble::tibble(id = rb_clust$zones, agent_id = 1:length(rb_clust$zones)) |>
  tidyr::unnest(cols = id)

if (!"score_variance" %in% colnames(inst$points)) {
  inst$points <- inst$points |>
    dplyr::left_join(rb_clust$variances, by = c("id"))
}

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

ggplot2::ggplot() +
  ggplot2::geom_segment(
    data = same_zone_edges,
    ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
    color = ggplot2::alpha("black", 0.3), linetype = "dashed"
  ) +
  ggplot2::geom_point(
    data = inst$points |> dplyr::filter(point_type == "terminal"),
    ggplot2::aes(x, y), color = "red", shape = 17
  ) +
  ggplot2::geom_segment(
    data = route_segments,
    ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
  ) +
  ggplot2::geom_point(
    data = inst$points |>
      dplyr::filter(point_type == "intermediate") |>
      dplyr::left_join(temp, by = c("id")),
    ggplot2::aes(x, y, color = as.character(agent_id), size = score, alpha = score_variance)
  ) +
  # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
  ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
  ggplot2::theme_bw() +
  ggplot2::guides(
    shape = "none",
    fill = "none",
    color = "none",
    alpha = "none",
    size = "none"
  )
