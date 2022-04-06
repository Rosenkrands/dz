# library(dz)
# set.seed(1)

# Parameters
inst = dz::test_instances$p7_chao; L = 150; variances = generate_variances(inst = inst); info = generate_information(inst, r = 20)

# Generation of initial routes
message("Generating the initial routes")
routes <- 1:200 |> as.list() |> pbapply::pblapply(function(x) {initial_route(inst, L, variances, info)})

inst$points <- inst$points |>
  dplyr::left_join(variances, by = c("id")) # Join variances on points tibble

# Perform the clustering
message("Performing the clustering")
compute_dissimilarity <- function(i,j) {
  nodes_i <- unique(routes[[i]])
  nodes_j <- unique(routes[[j]])
  difference <- setdiff(nodes_i, nodes_j)
  return(length(difference))
}

n <- length(routes)
dissimilarity <- matrix(nrow = n, ncol = n)
for (i in 1:n) {
  for (j in 1:n) {
    dissimilarity[i,j] <- compute_dissimilarity(i,j)
  }
}

k = 3
hc <- stats::hclust(as.dist(dissimilarity))
cluster <- cutree(hc, k)

# Investigate where to place disputed points, based on most frequent use
route_info <- tibble::tibble(id = routes, cluster, route_id = 1:length(routes))

route_count <- route_info |>
  dplyr::group_by(cluster) |>
  dplyr::summarise(n_route = dplyr::n_distinct(route_id))

node_usage <- route_info |>
  tidyr::unnest(cols = id) |>
  dplyr::group_by(id, cluster) |>
  dplyr::summarise(n = dplyr::n()) |> # should maybe correct for number of routes
  dplyr::left_join(route_count, by = c("cluster")) |>
  dplyr::mutate(n = n / n_route) |>
  dplyr::summarise(num_cluster_use = dplyr::n_distinct(cluster),
                   most_frequent = dplyr::first(cluster, order_by = -n)) |>
  dplyr::mutate(disputed = ifelse(num_cluster_use > 1, 1, 0)) |>
  dplyr::ungroup()

# # for plotting
# route_segments <- tibble::tibble(routes, cluster) |>
#   tidyr::unnest(routes) |>
#   dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
#   dplyr::filter(!is.na(id_start)) |>
#   dplyr::select(-routes) |>
#   dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
#                     by = c("id_start" = "id")) |>
#   dplyr::inner_join(inst$points |> dplyr::select(id, x, y),
#                     by = c("id_end" = "id"), suffix = c("","end")) |>
#   dplyr::group_by(cluster,x,y,xend,yend) |>
#   dplyr::summarise(n = dplyr::n())

# # Plot the segment on the existing plot
# ggplot2::ggplot() +
#   # ggplot2::geom_segment(
#   #   data = inst$edges,
#   #   ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
#   #   color = ggplot2::alpha("black", 0.3), linetype = "dashed"
#   # ) +
#   ggplot2::geom_segment(
#     data = route_segments,
#     ggplot2::aes(x=x, y=y, xend=xend, yend=yend, color = as.character(cluster)),
#   ) +
#   ggplot2::geom_point(
#     data = inst$points |> dplyr::filter(point_type == "terminal"),
#     ggplot2::aes(x, y), color = "red", shape = 17
#   ) +
#   ggplot2::geom_point(
#     data = inst$points |>
#       dplyr::filter(point_type == "intermediate") |>
#       dplyr::inner_join(node_usage, by = c("id")),
#     ggplot2::aes(x, y, shape = as.character(disputed))
#   ) +
#   ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
#   ggplot2::theme_bw() +
#   ggplot2::guides(
#     shape = "none",
#     fill = "none",
#     color = "none"
#   )


# Assign disputed points to clusters
# First we find points that are only used by one cluster
message("resolving zoning conflicts")
zones <- list()

for (i in 1:k) {
  # add the most frequent undisputed points to each zone
  ids <- node_usage |>
    # dplyr::filter(most_frequent == i, disputed == 0) |>
    dplyr::filter(most_frequent == i) |>
    dplyr::pull(id)

  zones[[i]] <- unique(c(1, ids))
}

# Determine if a zone is connected
connected <- function(zone) {
  # zone <- zones[[1]]
  sub_g <- igraph::induced_subgraph(inst$g, vids = zone)
  igraph::is_connected(sub_g, mode = "weak") # check for undirected path between pairs of vertices
}

# Check the connectedness of each zone
lapply(zones, connected)

# plot the initial zones
# function to plot zones list
plot_zones <- function() {
  temp <- tibble::tibble(id = zones, agent_id = 1:k) |>
    tidyr::unnest(cols = id)

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = inst$edges,
      ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
      color = ggplot2::alpha("black", 0.3), linetype = "dashed"
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::geom_point(
      data = inst$points |>
        dplyr::filter(point_type == "intermediate") |>
        dplyr::left_join(node_usage, by = c("id")) |>
        dplyr::left_join(temp, by = c("id")) |>
        # dplyr::mutate(agent_id = tidyr::replace_na(agent_id, 0),
        #               disputed = tidyr::replace_na(disputed, 0)),
        dplyr::mutate(disputed = tidyr::replace_na(disputed, 0)),
      ggplot2::aes(x, y, shape = as.character(disputed), color = as.character(agent_id), size = score, alpha = score_variance)
    ) +
    # ggplot2::scale_color_manual(values = c("black", scales::hue_pal()(k))) +
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none",
      color = "none"
    )
}
plot_zones()

# de-zone points that are not connected to the source through its own zone
available_nodes <- integer()

for (i in 1:k) {
  # Make the sub graph induced by each zone
  sub_g <- igraph::induced_subgraph(inst$g, vids = zones[[i]])
  if (!igraph::is_connected(sub_g, mode = "weak")) {
    # The problem is the nodes that are not connected to the source,
    # nodes that are not connected to the source will have distance == Inf
    temp_dst <- igraph::distances(sub_g, v = 1) # calculate distances
    unconnected <- igraph::V(sub_g)[temp_dst == Inf] |> # get the node id for nodes with distance == Inf
      names() |> as.integer()

    # Idea: discard the nodes from the zone and see if we can add them to another zone
    available_nodes <- append(available_nodes, unconnected)
    zones[[i]] <- zones[[i]][!zones[[i]] %in% unconnected]
  }
}

# Plot zones with isolated points removed from their respective zone
plot_zones()

# iterate through the available nodes and see if we can find another zone for them
# IDEA: this could be done using the local search components from the clustering setup
n_available_nodes <- length(available_nodes)
message(paste0("found ", n_available_nodes, " conflicts"))

while (length(available_nodes) > 0) {
  for (i in 1:k) {
    # print(paste0("zone is: ", i))
    # check along the way if we have assigned all available nodes
    if (length(available_nodes) == 0) break

    added_nodes <- integer()
    for (j in 1:length(available_nodes)) {
      # print(paste0("j is: ", j))
      # print(paste0("available nodes is: ", available_nodes))
      if (length(available_nodes) == 0) break

      # temp_zone is zone i along with the next available node
      temp_zone <- append(zones[[i]], available_nodes[j])
      sub_g <- igraph::induced_subgraph(inst$g, vids = temp_zone)
      if (igraph::is_connected(sub_g, mode = c("weak"))) {
        # print(paste0("The node ", available_nodes[j], " can be added"))
        # if temp_zone is weakly connected we add the available node to this zone
        zones[[i]] <- temp_zone
        added_nodes <- append(added_nodes, available_nodes[j])
        if (length(available_nodes) == 0) break
      }
    }
    # print(paste0("added nodes are: ", added_nodes))
    available_nodes <- available_nodes[!available_nodes %in% added_nodes]
  }
}
message("conflicts resolved")
message("all done")

# Plot the zones again with the points added to other zones
plot_zones()
