library(igraph)

inst <- test_instances$p8_christofides
info <- generate_information(inst, r = 20)
p_inst <- prepare_instance(inst, info)
# plot(p_inst)

plot_multiple_routes <- function(routes, label = "point") {
  # For testing purposes:

  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = routes, route_id = factor(1:length(routes))) |>
    tidyr::unnest(cols = route) |>
    dplyr::group_by(route_id) |>
    dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-route) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end")) |>
    dplyr::group_by(route_id) |>
    dplyr::mutate(n = runif(1), across(x:yend, ~ .x + n))

  p <- ggplot2::ggplot()

  if (label == "point") {
    p <- p +
      ggplot2::geom_point(
        data = p_inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, size = score, shape = point_type), alpha = .5
      )
  } else if (label == "text") {
    p <- p +
      ggplot2::geom_text(
        data = p_inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, label = id)
      )
  }

  p <- p +
    ggplot2::geom_segment(
      data = p_inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color = route_id),
      size = 1, alpha = .6
    ) +
    ggplot2::geom_point(
      data = p_inst$points |> dplyr::filter(point_type %in% c("source", "sink")),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::ggtitle(paste0("Instance: ", p_inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::coord_fixed()

  return(p)
}

# add edges
nghbr <- igraph::neighborhood(inst$g,order = 1)

new_g <- inst$g

# lapply(
#   igraph::V(inst$g) |> names() |> as.integer(),
#   function(id) {
#     nghbr_id <- nghbr[[id]][-1] |> names() |> as.integer()
#     nghbr_nghbr <- unique(do.call(c, lapply(nghbr_id, function(x){
#       nghbr[[x]][-1] |> names() |> as.integer()
#     })))
#     edges_to_add <- nghbr_nghbr[(!nghbr_nghbr %in% nghbr_id) & (nghbr_nghbr != id)]
#
#     dist <- sapply(edges_to_add, function(to){
#       sqrt((inst$points$x[id] - inst$points$x[to])^2 + (inst$points$y[id] - inst$points$y[to])^2)
#     })
#
#     new_g <<- igraph::add_edges(
#       new_g,
#       edges =
#       as.character(c(
#         rbind(
#           rep(id,length(edges_to_add)),
#           matrix(edges_to_add, ncol=length(edges_to_add))
#         )
#       )),
#       attr = list("weight" = dist)
#     )
#   }
# )
#
# plot(new_g)

inst$g <- new_g
plot(inst, label = "text")

# id = 1
# nghbr_id <- nghbr[[id]][-1] |> names() |> as.integer()
# nghbr_nghbr <- unique(do.call(c, lapply(nghbr_id, function(x){
#   nghbr[[x]][-1] |> names() |> as.integer()
# })))
# nghbr_nghbr[(!nghbr_nghbr %in% nghbr_id) & (nghbr_nghbr != id)]

source_sink_idea <- function() {
  routes <- list()
  g <- as.undirected(inst$g, mode = "collapse")
  i = 1
  cat(i, '\r')

  while (i < 7) {
    route <- igraph::shortest_paths(graph = g, from = "1", to = as.character(nrow(inst$points)), algorithm = "dijkstra")$vpath[[1]] |> names() |> as.integer()

    routes[[i]] <- route

    print(plot_multiple_routes(routes) + ggplot2::theme(legend.position = "none"))

    nodes_to_remove <- route[2:(length(route) - 1)]

    g <- igraph::delete_vertices(g, v = as.character(nodes_to_remove))

    i <- i + 1
    cat(i, '\r')
  }
  cat('\n')
}

animation::saveGIF(
  source_sink_idea(),
  interval = 1,
  ani.width = 1080,
  ani.height = 1080,
  ani.res = 250,
  loop = 1
)



dst <- distances(inst$g)

route_length <- function(x) {
  route <- stats::embed(x, 2)[, 2:1]
  sum(dst[route])
}

plot(sapply(routes, route_length), type = "l")
