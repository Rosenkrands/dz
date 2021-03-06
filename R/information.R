#' Generate an information set
#'
#' Given an instance the function will generate a an information matrix.
#'
#' @param inst A list returned from the `instance` function
#' @param r The maximum radius information can travel
#'
#' @return a matrix of information
#' @export
#'
generate_information <- function(inst, r = 20) {
  # For testing purposes:
  # inst <- test_instances$p7_chao; r = 20

  # generate information matrix
  eps <- matrix(
    data = runif(nrow(inst$points)^2, min = -1, max = 1),
    nrow = nrow(inst$points)
  )

  info <- (eps / inst$dst)
  info[!is.finite(info)] <- 0
  info[inst$dst > r] <- 0

  # scale according to expected score
  info <- info * inst$points$score

  return(info)
}
