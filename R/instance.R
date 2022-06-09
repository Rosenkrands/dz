#' Instantiate a test instance
#'
#' `instance` is used to instantiate a test instance.
#' There are some test instances included in the package, both as raw data and as instance objects.
#' This function is used to convert the raw data to instance objects.
#'
#' @param path_to_file Path to the `.txt` file holding the instance data
#'
#' @return `instance` returns an object of \code{\link{class}} "`instance`".
#'   An object of class "`instance`" is a list containing:
#'   `points` a `tibble` containing coordinates and scores for each point.
#'   `edges` a `tibble` containing the (weighted) edges in the graph.
#'   `g` an igraph object of the graph making up the instance.
#'   `dst` a matrix containing the length of shortest paths between points.
#'   `n` an integer equal to the number of points in the instance.
#' @export
#'
instance <- function(path_to_file) {
  # path_to_file = "inst/extdata/test_instances/chao/p7.txt"
  points <- utils::read.table(path_to_file) |>
    tibble::tibble() |>
    dplyr::rename(x = V1, y = V2, score = V3) |>
    dplyr::distinct(x, y, .keep_all = T) |>
    dplyr::mutate(id = dplyr::row_number(),
                  point_type = ifelse(id == 1, "source", ifelse(id == max(id), "sink", "node")))

  # Compute edges in delaunay triangulation
  tri <- (deldir::deldir(points$x, points$y))$delsgs

  # construct the igraph object
  tri$dist <- sqrt((tri$x1 - tri$x2)^2 + (tri$y1 - tri$y2)^2) # We assume euclidean distance

  g <- igraph::graph.data.frame(
    tri |> dplyr::select(ind1, ind2, weight = dist),
    directed = FALSE,
    vertices = points |> dplyr::select(id, score)
  )

  # calculate distance matrix (based on shortest path)
  dst <- igraph::distances(g, algorithm = "dijkstra")

  if (!igraph::is_connected(g)) {
    # find neighbors of source node
    nghbrs <- igraph::neighborhood(g, order = 1, nodes = "1")[[1]] |> names() |> as.integer()

    # construct the edges to add, i.e. sink should have the same edges as source
    edges_to_add <- do.call(
      c,
      lapply(nghbrs[-1], function(nghbr) c(max(points$id), nghbr))
    )

    g <- igraph::add_edges(g, edges_to_add, attr = list("weight" = dst[1, nghbrs[-1]]))
    dst <- igraph::distances(g, algorithm = "dijkstra")
  }

  n <- nrow(points)

  name <- path_to_file |>
    tools::file_path_sans_ext() |>
    (function(x) stringr::str_split(x, "/")[[1]])() |>
    utils::tail(n = 2) |> rev() |> paste(collapse = "_")

  structure(
    list(
      "points" = points,
      "edges" = tri,
      "g" = g,
      "dst" = dst,
      "n" = n,
      "name" = name
    ),
    class = "instance"
  )
}

#' Plot method for instance object
#'
#' @param inst Object of class `instance`
#' @param delaunay Show edges from a Delaunay triangulation
#'
#' @return A ggplot object
#' @export
#'
plot.instance <- function(inst, delaunay = T, label = "point") {
  # Instantiate the ggplot object
  p <- ggplot2::ggplot()

  # Add delaunay edges
  if (delaunay) {
    p <- p +
      ggplot2::geom_segment(
        data = inst$edges,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      )
  }

  # Add points and title to the plot
  if (label == "point") {
    p <- p +
      ggplot2::geom_point(
        data = inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, size = score, color = score, shape = point_type)
      )
  } else if (label == "text") {
    p <- p +
      ggplot2::geom_text(
        data = inst$points |> dplyr::filter(point_type == "node"),
        ggplot2::aes(x, y, label = id)
      )
  }

  p <- p +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type %in% c("source", "sink")),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    # ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none", size = "none") +
    ggplot2::labs(x = "x", y = "y") +
    ggplot2::coord_fixed()

  return(p)
}
