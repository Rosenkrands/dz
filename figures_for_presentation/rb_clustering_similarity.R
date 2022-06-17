library(dz)
library(tidyverse)
pbapply::pboptions(use_lb = T)

inst <- test_instances$p7_chao
# plot(inst, label = "text")

# plot(inst$g)

info <- generate_information(inst, r = 20)
p_inst <- prepare_instance(inst, info)
# plot(p_inst)

L = 100
k = 3
num_routes = 1000

num_cores <- parallel::detectCores(logical = F)
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, c('p_inst', 'inst', 'L'))
invisible(parallel::clusterEvalQ(cl, {library(dz)}))

greedy_routes <- pbapply::pblapply(
  1:num_routes,
  function(x) greedy_route(p_inst, zone_id = 0, L = L, info, top_percentile = .5, nghbr_order = 1),
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

plot_multiple_routes <- function(routes, mad, label = "point") {
  # For testing purposes:

  # Generate route segments based on the route
  route_segments <- tibble::tibble(route = lapply(routes, function(x) x$route), route_id = factor(1:length(routes))) |>
    tidyr::unnest(cols = route) |>
    dplyr::group_by(route_id) |>
    dplyr::mutate(id_start = dplyr::lag(route), id_end = route) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-route) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end")) |>
    dplyr::group_by(route_id) # |>
    # dplyr::mutate(n = runif(1), across(x:yend, ~ .x + n))

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
    # ggplot2::ggtitle(paste0("MAD = ", mad)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "none")

  return(p)
}

# plot_multiple_routes(routes = list(greedy_routes[[220]]))

base_route = which.max(sapply(greedy_routes, function(x) x$expected_score))

sim_w_base <- dissimilarity[base_route, ]
names(sim_w_base) <- 1:num_routes

sim_w_base <- sort(sim_w_base)

sim_unique <- sim_w_base[names(sim_w_base)[!duplicated(sim_w_base)]]

equally_spaced_points <- seq(min(sim_unique), max(sim_unique), length.out = 50)

sim_unique <- sim_unique[
  sapply(equally_spaced_points, function(i){
    names(which.min(abs(sim_unique - i)))
  })
]

sim_unique <- sim_unique[!duplicated(sim_unique)]

plot_sim <- function(id) {
  mad <- sim_unique[as.character(id)]
  route_plot <- plot_multiple_routes(routes = list(greedy_routes[[base_route]], greedy_routes[[id]]), round(mad, 1))

  mad_plot <- ggplot() +
    geom_col(aes(y = 0, x = mad)) +
    theme_bw() +
    scale_x_continuous(limits = c(min(sim_unique), max(sim_unique))) +
    scale_y_discrete(labels = NULL, breaks = NULL) + labs(y = "" , x = "MAD")

  cowplot::plot_grid(
    route_plot,
    mad_plot,
    ncol = 1,
    rel_heights = c(1, .3)
  )
}

mad_animation <- function() {
  for (id in 1:length(sim_unique)) {
    cat(id, 'of', length(sim_unique), '\r')
    print(plot_sim(as.integer(names(sim_unique)[id])))
  }
  cat('\n')
}

animation::saveGIF(
  mad_animation(),
  interval = .4,
  ani.width = 1080,
  ani.height = 1620,
  ani.res = 250
)
