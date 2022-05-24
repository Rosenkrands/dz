library(dz)
set.seed(7)

# Setting parameters
inst <- test_instances$p7_chao
L <- 100
variances <- generate_variances(inst)
info <- generate_information(inst)
num_routes <- 100

# Prepare instance
p_inst <- prepare_instance(inst, variances, info)
# (p <- plot(p_inst))

message("Construct the initial rotues")
suppressMessages(
  init_routes <- 1:num_routes |>
    as.list() |>
    pbapply::pblapply(function(x) initial_route2(p_inst, L, info))
)

position_dissimilarity <- function(ids) {
  # Determine the longest route
  routes <- list(init_routes[[ids[1]]]$route, init_routes[[ids[2]]]$route)
  longest_route <- do.call(c, lapply(routes, length)) |> which.max()

  sapply(
    routes[[longest_route]],
    function(node_id) {
      p_inst$dst[node_id, unique(routes[-longest_route][[1]])] |> min()
    }
  ) |> mean()
}

# find all combinations of routes and compute dissimilarity
combinations <- utils::combn(1:num_routes, 2, simplify = F)
dissimilarity <- pbapply::pblapply(combinations, position_dissimilarity)

# element-based dissimilarity
compute_dissimilarity <- function(ids) {
  nodes_i <- unique(init_routes[[ids[1]]]$route)
  nodes_j <- unique(init_routes[[ids[2]]]$route)
  difference <- setdiff(nodes_i, nodes_j)
  return(length(difference))
}

el_dissimilarity <- pbapply::pblapply(combinations, compute_dissimilarity)

# create matrix to hold results
dis_mat <- matrix(data = 0, nrow = num_routes, ncol = num_routes)

# take dissimilarity values from the list and insert into the matrix
lapply(seq_along(combinations), function(i) {
  ids <- combinations[[i]]
  dis_mat[ids[1], ids[2]] <<- dissimilarity[[i]]
})

# mirror the upper part of the matrix into the lower part
dis_mat <- dis_mat + t(dis_mat)

plot_two_routes <- function(route1, route2) {
  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = c(route1, route2), route_id = c(rep(1, length(route1)), rep(2, length(route2)))) |>
    dplyr::group_by(route_id) |>
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
    ggplot2::guides(shape = "none", size = "none", color = "none")
}

dis_vec <- do.call(c, dissimilarity)
names(dis_vec) <- seq_along(dis_vec)
dis_vec <- sort(dis_vec)

# element-based
el_dis_vec <- do.call(c, el_dissimilarity)
names(el_dis_vec) <- seq_along(el_dis_vec)
el_dis_vec <- sort(el_dis_vec)

i = 1
value <- dis_vec[i]
ids <- combinations[[names(dis_vec[i]) |> as.integer()]]
route1 <- init_routes[[ids[1]]]$route; route2 <- init_routes[[ids[2]]]$route
plot_two_routes(route1, route2) + ggplot2::ggtitle(label = paste0("element-based dissimilarity: ", round(value, 3)), subtitle = paste(ids, sep = " - "))
i = i + 1

# element-based
i = 1
value <- el_dis_vec[i]
ids <- combinations[[names(el_dis_vec[i]) |> as.integer()]]
route1 <- init_routes[[ids[1]]]$route; route2 <- init_routes[[ids[2]]]$route
plot_two_routes(route1, route2) + ggplot2::ggtitle(label = paste0("EBD = ", round(value, 3), " PBD = ", round(dis_mat[ids[1], ids[2]], 3)), subtitle = paste(ids, collapse = " - "))
i = i + 1

for (i in 1:length(combinations)) {if (identical(combinations[[names(el_dis_vec[i]) |> as.integer()]], c(39L,54L))) print(i)}

# example is 39-54 and 39-53 (with set.seed(7))
dis_mat[39,54]; dis_mat[39,53]

dis_vec[as.character(i)]
