#' Simulate disease trajectories from subpopulations.
#'
#' @import fda
#' @import ggplot2
#' @import splines
#'
#' @docType package
#' @name trajsim
#' @aliases trajsim trajsim-package
NULL

# bspline_basis -----------------------------------------------------------

#' @export
bspline_basis <- function(x_range, num_bases) {
  chunk_len <- diff(x_range) / num_bases
  lower <- x_range[1] - chunk_len
  upper <- x_range[2] + chunk_len

  basis <- create.bspline.basis(c(lower, upper), num_bases)
  basis$orig_x_range <- x_range
  class(basis) <- c("bspline_basis", class(basis))
  basis
}

#' @export
simulate.bspline_basis <- function(basis, n = 1) {
  coef_mean <- rep(0, basis$nbasis)
  coef_covm <- diag(basis$nbasis)
  coefficients <- mvtnorm::rmvnorm(n, coef_mean, coef_covm)
  funcs <- lapply(seq(n), function(ix) {
    bspline_func(coefficients[ix, ], basis)
  })
  funcs
}

# bspline_func ------------------------------------------------------------

#' @export
bspline_func <- function(coefficients, basis) {
  func <- structure(list(), class = "bspline_func")
  func$coefficients <- coefficients
  func$basis <- basis
  func
}

#' @export
predict.bspline_func <- function(bspline_fn, x) {
  X <- predict(bspline_fn$basis, x)
  y <- X %*% bspline_fn$coefficients
  as.numeric(y)
}

#' @export
plot.bspline_func <- function(bspline_fn, layer = FALSE) {
  x_range <- bspline_fn$basis$rangeval
  x <- seq(x_range[1], x_range[2], length = 100)
  y <- predict(bspline_fn, x)
  func_data <- data.frame(x = x, y = y)
  curve <- geom_line(aes(x, y), data = func_data)

  if (layer)
    curve
  else
    ggplot() + curve + xlim(bspline_fn$basis$orig_x_range)
}