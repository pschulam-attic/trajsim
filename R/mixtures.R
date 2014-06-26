
# bspline_mixture ---------------------------------------------------------

#' @export
bspline_mixture <- function(num_groups, basis, seed = 1) {
  set.seed(seed)
  mix_probs <- as.numeric(MCMCpack::rdirichlet(1, rep(1, num_groups)))
  bsplines <- simulate(basis, num_groups)
  mixture <- structure(list(), class = "bspline_mixture")
  mixture$num_groups <- num_groups
  mixture$basis <- basis
  mixture$probs <- mix_probs
  mixture$bsplines <- bsplines
  mixture
}

#' @export
plot.bspline_mixture <- function(bmix) {
  p <- ggplot()
  for (bsp in bmix$bsplines)
    p <- p + plot(bsp, layer = TRUE)

  p <- p + xlim(bmix$basis$orig_x_range)
  p
}

#' @export
simulate.bspline_mixture <- function(bmix, n, num_points, obs_model, noise_model) {

  curves <- lapply(seq(n), function(idx) {
    group <- sample(bmix$num_groups, 1, prob = bmix$probs)
    x <- simulate(obs_model, num_points)
    y <- predict(bmix$bsplines[[group]], x)
    y <- y + simulate(noise_model, x)
    data.frame(idx = idx, x = x, y = y, group = group)
  })

  do.call("rbind", curves)
}