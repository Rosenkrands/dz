#' Generate routes for a clustered instance
#'
#' Given a clustered instance and a routing method the function will provide routes for the given instance.
#'
#' @param clust A list returned from the `clustering` function
#' @param routing_method A method from which to perform the routing
#'
#' @return A list
#' @export
#'
routing <- function(clust, routing_method = "random") {
  # For testing purposes:
  # clust = clustering(test_instances$p7_chao, k = 4, cluster_method = "pam"); routing_method = "random"

  # First we should find the closest point in each cluster to the source node
  first_point <- clust$instance$points |>
    dplyr::mutate(dist_to_source = sqrt((x - x[1])^2 + (y - y[1])^2)) |> # Calculate the distance to the source node
    dplyr::group_by(zone) |>
    dplyr::filter(dist_to_source == min(dist_to_source), # Get smallest distance by zone,
                  !is.na(zone)) # Remove the source node

  if ((nrow(first_point) != clust$k)) {stop("number of rows in first node to visit is not equal to number of zones")}

  # Then we should find the closest point in each cluster to the sink node
  last_point <- clust$instance$points |>
    dplyr::mutate(dist_to_source = sqrt((x - x[nrow(clust$instance$points)])^2 + (y - y[nrow(clust$instance$points)])^2)) |> # Calculate the distance to the sink node
    dplyr::group_by(zone) |>
    dplyr::filter(dist_to_source == min(dist_to_source), # Get smallest distance by zone,
                  !is.na(zone))

  if ((nrow(last_point) != clust$k)) {stop("number of rows in last node to visit is not equal to number of zones")}

  # Create a tibble to hold the routes
  routes <- tibble::tibble(agent_id = 1:clust$k)

  generate_routes <- function(agent_id) {
    # For testing purposes:
    # agent_id = 4

    route = integer()

    # First point is the source node
    route <- append(route, 1)

    # Determine first and last zone point for the agent
    first_point_in_zone <- first_point |>
      dplyr::filter(zone == agent_id) |>
      dplyr::pull(id)

    last_point_in_zone <- last_point |>
      dplyr::filter(zone == agent_id) |>
      dplyr::pull(id)

    # Add first zone point for the agent
    route <- append(
      route,
      first_point_in_zone
    )

    # Add random points to the route
    route <- append(
      route,
      # Find some random point in the zone for illustrative purposes
      clust$instance$points |>
        dplyr::filter(
          zone == agent_id,
          !id %in% c(first_point_in_zone, last_point_in_zone)
        ) |>
        dplyr::pull(id) |>
        sample(size = 4)
    )

    # Determine last zone point for the agent
    route <- append(
      route,
      last_point_in_zone
    )

    # Last point is the terminal node
    route <- append(route, nrow(clust$instance$points))

    return(route)
  }

  routes <- routes |>
    dplyr::mutate(routes = lapply(as.list(routes$agent_id), generate_routes))

  structure(
    list(
      "instance" = clust$instance,
      "routes" = routes,
      "k" = clust$k,
      "cluster_method" = clust$cluster_method,
      "in_points" = clust$in_points
    ),
    class = "routing"
  )
}


#' Plot method for a routing object
#'
#' Visualize the generated routes
#'
#' @param rout A list returned from the `routing` function
#'
#' @return A ggplot object
#' @export
#'
plot.routing <- function(rout) {
  # Determine segments from routes to plot
  route_segments <- rout$routes |>
    tidyr::unnest(routes) |>
    dplyr::group_by(agent_id) |>
    dplyr::mutate(id_start = dplyr::lag(routes), id_end = routes) |>
    dplyr::filter(!is.na(id_start)) |>
    dplyr::select(-routes) |>
    dplyr::inner_join(rout$instance$points |> dplyr::select(id, x, y),
                      by = c("id_start" = "id")) |>
    dplyr::inner_join(rout$instance$points |> dplyr::select(id, x, y),
                      by = c("id_end" = "id"), suffix = c("","end"))

  # Plot the segment on the existing plot
  ggplot2::ggplot() +
    ggalt::geom_encircle(
      data = rout$in_points,
      ggplot2::aes(x = x, y = y, group = zone, fill = as.character(zone)),
      s_shape = 1, expand = 0, alpha = 0.2
    ) +
    ggplot2::geom_point(
      data = rout$instance$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color = score, shape = point_type)
    ) +
    ggplot2::geom_segment(
      data = route_segments,
      ggplot2::aes(x=x, y=y, xend=xend, yend=yend)
    ) +
    ggplot2::geom_point(
      data = rout$instance$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::ggtitle(paste0("Instance: ", rout$instance$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(
      shape = "none",
      fill = "none"
    )
}
