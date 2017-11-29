context("uplift")

test_that("trapz of normal is 1", {
  x <- seq(-10, 10, by = 0.1)
  y <- exp(-0.5 * x ** 2) / sqrt(2 * pi)

  expect_equal(trapz(x, y), 1, tolerance = 1e-7)
})
