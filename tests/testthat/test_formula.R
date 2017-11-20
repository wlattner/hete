context("formula parsing")

test_that("parse_hete_formula removes treatment from model", {
  d <- list(x1 = 1, x2 = 3, y = 0, treatment = 0)
  parsed <- parse_hete_formula(y ~ . | treatment, data = d)

  expect_identical(colnames(parsed$x), c("x1", "x2"))
})
