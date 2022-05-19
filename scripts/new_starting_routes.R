library(dz)
set.seed(1)

# setup the initial variables and clustering
inst <- test_instances$p7_chao
L <- 100
k <- 4
variances <- generate_variances(inst)
info <- generate_information(inst)

p_inst <- prepare_instance(inst, variances, info)

rb_clust <- rb_clustering(p_inst, L, k, num_routes = 100, info)
zones <- rb_clust$zones
plot(rb_clust)

# helper functions
sp <- function(id1, id2, graph = g){
  # handle identical ids
  if (id1 == id2) {
    warning("Trying to calculate the shortest path from one node to itself, returning empty vector")
    return(integer())
  }

  # Find vertices that make up the path
  short_vert <- igraph::shortest_paths(
    graph = graph,
    from = as.character(id1),
    to = as.character(id2),
    output = "vpath"
  )$vpath[[1]] |>
    names() |>
    as.integer()

  # return the path not including the first point
  return(short_vert |> tail(-1))
}
route_length <- function(route){
  # if (identical(route, c(1,1))) return(0)
  sum(sub_dst[stats::embed(as.character(route), 2)[, 2:1]])
}

plot_progress <- function() {
  temp <- tibble::tibble(id = zones, agent_id = 1:length(zones)) |>
    tidyr::unnest(cols = id)

  inst$points$score <- score

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

  route_segments <- tibble::tibble(
    routes = route,
    route_type = rep("route", length(route))
  ) |>
    # dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = same_zone_edges |> dplyr::filter(zone == zone_id),
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate", id %in% zones[[zone_id]]) |>
        dplyr::left_join(temp, by = c("id")),
      # ggplot2::aes(x, y, color = as.character(agent_id), size = score)
      ggplot2::aes(x, y, size = score), color = "darkgrey"
    ) +
    ggplot2::geom_text(
      data = inst$points |> dplyr::filter(point_type == "intermediate", id %in% zones[[zone_id]]),
      ggplot2::aes(x, y, label = id), nudge_x = .25, nudge_y = 1, size = 2.5
    ) +
    ggplot2::geom_text(
      data = inst$points |> dplyr::filter(point_type == "intermediate", id %in% zones[[zone_id]]),
      ggplot2::aes(x, y, label = round(score, 1)), nudge_x = -.35, nudge_y = 1, size = 2, color = "blue"
    ) +
    ggplot2::geom_segment(
      data = route_segments |> dplyr::filter(route_type == "route"),
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend), color = "black", size = .75
    ) +
    ggplot2::geom_segment(
      data = route_segments |> dplyr::filter(route_type == "remaining_route"),
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend), color = "magenta", linetype = "dashed", size = .75
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17, size = 3
    ) +
    ggplot2::ggtitle(
      paste0("Instance: ", inst$name),
      subtitle = paste0("id_now: ", id_now, "\n",
                        "route: ", paste(route, collapse = " "), "\n",
                        "L_remaining: ", round(L_remaining, 1))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none",
      alpha = "none",
      size = "none"
    )
}

# construct the starting routes
zone_id <- 1

# subgraph for the zone and distances
sub_g <- igraph::induced_subgraph(p_inst$g, vids = zones[[zone_id]]) # plot(sub_g)
sub_dst <- igraph::distances(sub_g)

route <- c(1,1)
L_remaining <- L

# Make copies of variables to alter during route generation
score <- p_inst$points$score
realized_score <- p_inst$points$realized_score
unexpected <- p_inst$points$unexpected

plot_progress()

### iterate from here ###
id_now <- tail(route, 1); cat("id_now is", id_now, "\n")
current_score <- sum(inst$points$score[unique(route)])

# update score
score[id_now] <- 0; realized_score[id_now] <- 0

candidate_routes <- lapply(zones[[zone_id]][-1], function(x){
  print(x)
  first_part <- c(route[1:length(route)-1], sp(route[length(route)-1],x, g = sub_g))
  route_temp <- c(first_part, sp(tail(first_part, 1), 1, g = sub_g))
  L_temp <- route_length(route_temp)
  score_temp <- sum(inst$points$score[unique(route_temp)])
  if ((L_temp <= L) & (score_temp > current_score)) route_temp
})

candidate_routes <- lapply(candidate_nodes, function(x){
  first_part <- c(route[1:length(route)-1], sp(tail(first_part, 1),x, g = sub_g))
  route_temp <- c(first_part, sp(tail(first_part, 1), 1, g = sub_g))

})

temp_score <- sapply(feasible_nghbrs, function(nghbr){
  temp_path <- sp(as.character(nghbr), remaining_route[2], g = sub_g)
  ids <- c(nghbr, temp_path[-length(temp_path)]) # remove remaining_route[2]
  sum(score[ids])
})
