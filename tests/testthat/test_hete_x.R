context("hete_x")

test_that("use `est` for all four models", {
  x <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)
  tmt <- rbinom(1, 100, prob = 0.5)

  mock_fit <- mockery::mock(cycle = TRUE)
  mock_predict <- mockery::mock(y, cycle = TRUE)
  est <- base_estimator(mock_fit, mock_predict)

  hete_x(x, y, tmt, est)

  expect_equal(length(mock_fit), 4)
})

test_that("use `est` for unspecified models", {
  x <- matrix(rnorm(100 * 10), nrow = 100)
  y <- rnorm(100)
  tmt <- rbinom(1, 100, prob = 0.5)

  mock_fit <- mockery::mock(cycle = TRUE)
  mock_predict <- mockery::mock(y, cycle = TRUE)
  est <- base_estimator(mock_fit, mock_predict)

  mock_fit2 <- mockery::mock()
  mock_predict2 <- mockery::mock(y)
  est2 <- base_estimator(mock_fit2, mock_predict2)

  hete_x(x, y, tmt, est, ctl_est = est2)

  expect_equal(length(mock_fit), 3)
  expect_equal(length(mock_fit2), 1)
})
