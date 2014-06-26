
# uniform_observations ----------------------------------------------------

#' @export
uniform_observations <- function(x_range) {
  obs <- structure(list(), class = "uniform_observations")
  obs$x_range <- x_range
  obs
}

#' @export
simulate.uniform_observations <- function(obs, n) {
  lower <- obs$x_range[1]
  upper <- obs$x_range[2]
  sort(runif(n, lower, upper))
}