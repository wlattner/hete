context("hete_split")

test_that("use `est` for both models", {
  d <- fake_problem()
  est <- mock_estimator()

  hete_split(d$x, d$y, d$tmt, est)
  expect_length(fit_calls(est), 2)
})

test_that("use specified ctl and tmt estimators", {
  d <- fake_problem()
  est1 <- mock_estimator()
  est2 <- mock_estimator()

  hete_split(d$x, d$y, d$tmt, tmt_est = est1, ctl_est = est2)

  expect_length(fit_calls(est1), 1)
  expect_length(fit_calls(est2), 1)
})
