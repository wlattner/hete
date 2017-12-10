mock_estimator <- function() {
  fit_calls <- list()
  fit_fn <- function(x, y) {
    fit_calls[[length(fit_calls) + 1]] <<- list(x = x, y = y)

    structure(list(), class = "mock_estimator")
  }

  predict_calls <- list()
  predict_fn <- function(m, x) {
    predict_calls[[length(predict_calls) + 1]] <<- list(x = x)

    runif(nrow(x))
  }

  base_estimator(fit_fn, predict_fn)
}

predict_calls <- function(m) {
  environment(environment(m)$predict_fn)$predict_calls
}

fit_calls <- function(m) {
  environment(environment(m)$fit_fn)$fit_calls
}
