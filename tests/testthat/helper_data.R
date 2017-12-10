fake_problem <- function() {
  x <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)
  tmt <- rbinom(100, 1, prob = 0.5)

  list(x = x, y = y, tmt = tmt)
}
