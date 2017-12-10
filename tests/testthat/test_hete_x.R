context("hete_x")

test_that("use `est` for all four models", {
  d <- fake_problem()
  est <- mock_estimator()

  hete_x(d$x, d$y, d$tmt, est)

  expect_length(fit_calls(est), 4)
})

test_that("use `est` for unspecified models", {
  d <- fake_problem()
  est1 <- mock_estimator()
  est2 <- mock_estimator()

  hete_x(d$x, d$y, d$tmt, est = est1, ctl_est = est2)

  expect_length(fit_calls(est1), 3)
  expect_length(fit_calls(est2), 1)
})
