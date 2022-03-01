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
#'   `n` an integer equal to the number of points in the instance.
#' @export
#'
#' @examples
#' # WIP
instance <- function(path_to_file) {
  points <- utils::read.table(path_to_file) |>
    tibble::tibble() |>
    dplyr::rename(x = V1, y = V2, score = V3) |>
    dplyr::mutate(id = dplyr::row_number(), .before = dplyr::everything(),
                  point_type = ifelse(score == 0, "terminal", "intermediate"))

  n <- nrow(points)

  name <- path_to_file |>
    tools::file_path_sans_ext() |>
    (function(x) stringr::str_split(x, "/")[[1]])() |>
    utils::tail(n = 2) |> rev() |> paste(collapse = "_")

  structure(list("points" = points, "n" = n, "name" = name), class = "instance")
}

#' Plot method for instance object
#'
#' @param inst Object of class `instance`
#' @param delaunay Show edges from a Delaunay triangulation
#' @param voronoi Show tiles from a Voronoi tesselation
#'
#' @return A ggplot object
#' @export
#'
plot.instance <- function(inst, delaunay = FALSE, voronoi = FALSE) {
  # Instantiate the ggplot object
  p <- ggplot2::ggplot()

  # If either delaunay or voronoi is true we compute the triangulation
  if (delaunay | voronoi) {
    tri <- deldir::deldir(inst$points$x, inst$points$y)
  }

  # Add delaunay edges
  if (delaunay) {
    p <- p +
      ggplot2::geom_segment(
        data = tri$delsgs,
        ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
        color = ggplot2::alpha("black", 0.3), linetype = "dashed"
      )
  }

  # Add voronoi tiles
  if (voronoi) {
    p <- p +
      ggvoronoi::stat_voronoi(
        data = inst$points |> dplyr::distinct(x,y),
        ggplot2::aes(x,y),
        geom = "path", color = ggplot2::alpha("black", 0.5)
      )
  }

  # Add points and title to the plot
  p <- p +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "intermediate"),
      ggplot2::aes(x, y, color = score, shape = point_type)
    ) +
    ggplot2::geom_point(
      data = inst$points |> dplyr::filter(point_type == "terminal"),
      ggplot2::aes(x, y), color = "red", shape = 17
    ) +
    ggplot2::ggtitle(paste0("Instance: ", inst$name)) +
    ggplot2::theme_bw() +
    ggplot2::guides(shape = "none")

  return(p)
}
