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
    dplyr::rename(x = V1, y = V2, score = V3)

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
#'
#' @return A ggplot object
#' @export
#'
plot.instance <- function(inst) {
  # print("this is plot.instance")
  ggplot2::ggplot(data = inst$points) +
    ggplot2::geom_point(ggplot2::aes(x, y, color = score)) +
    ggplot2::ggtitle(paste0("instance: ", inst$name)) +
    ggplot2::theme_bw()
}
