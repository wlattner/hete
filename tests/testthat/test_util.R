context("util")

test_that("error on non-factor 0/1 outcome", {
  y <- c(0, 1, 1, 0, 1, 1)

  expect_error(check_y(y), "y only has 1/0, to model a binary outcome, y must be a factor")
})
