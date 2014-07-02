
# uniform_observations ----------------------------------------------------

#' @export
uniform_observations <- function(x_range)
{
  obs <- structure(list(), class="uniform_observations")
  obs$x_range <- x_range
  obs
}

#' @export
simulate.uniform_observations <- function(obs, n)
{
  lower <- obs$x_range[1]
  upper <- obs$x_range[2]
  sort(runif(n, lower, upper))
}

# --------------------------------------------------------------------------
# bursty_observations

#' @export
bursty_observations <- function(xrange, lambda, priorsd, priorn)
{
  alpha <- priorn / 2
  beta <- alpha * priorsd^2

  obs <- structure(list(), class="bursty_observations")
  obs$xrange <- xrange
  obs$lambda <- lambda
  obs$alpha <- alpha
  obs$beta <- beta
  obs
}

#' @export
simulate.bursty_observations <- function(obs, n)
{
  nbursts <- 1 + rpois(1, obs$lambda)
  burstloc <- runif(nbursts, obs$xrange[1], obs$xrange[2])
  burstscale <- sqrt(MCMCpack::rinvgamma(nbursts, obs$alpha, obs$beta))
  burstprobs <- as.numeric(MCMCpack::rdirichlet(1, rep(1, nbursts)))

  xx <- numeric(n)
  nx <- 0

  while (nx < n)
  {
    z <- sample(nbursts, 1, prob=burstprobs)
    x <- rnorm(1, burstloc[z], burstscale[z])

    if (obs$xrange[1] <= x & x <= obs$xrange[2])
    {
      nx <- nx + 1
      xx[nx] <- x
    }
  }

  sort(xx)
}
