
# white_noise -------------------------------------------------------------

#' @export
white_noise <- function(sd = 1) {
  noise <- structure(list(), class = "white_noise")
  noise$sd <- sd
  noise
}

#' @export
simulate.white_noise <- function(noise, x) {
  rnorm(length(x), sd = noise$sd)
}

# gp_noise ----------------------------------------------------------------

#' @export
gp_noise <- function(bandwidth, ampl, noise) {

  kernel <- function(x, y) {
    d <- abs(x - y)
    k <- ampl^2 * exp(-1/2 * d^2 / bandwidth^2)
    k <- k + ifelse(x == y, noise^2, 0)
    k
  }

  gp <- structure(list(), class = "gp_noise")
  gp$kernel <- kernel
  gp
}

#' @export
simulate.gp_noise <- function(noise, x) {
  kern_mat <- outer(x, x, noise$kernel)
  as.numeric(rmvnorm(1, sigma = kern_mat))
}