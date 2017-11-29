#' Construct a Base Estimator
#'
#' This is a helper to bundle up the model fitting and prediction functions.
#' All the models in this package expect fit and predict functions which take
#' only data as arguments. \code{x} and \code{y} for fitting and only \code{x}
#' for prediction.
#'
#' @param fit_fn a function taking only two parameters, the predictor variables
#'   \code{x} and a vector of outcomes \code{y}. This function should return an
#'   object which has an implementation of \code{\link[stats]{predict}}.
#' @param predict_fn a function taking two parameters, the model \code{object}
#'   and the predictor variables \code{newdata}. \code{stats::predict} is
#'   used by default.
#'
#' @examples \dontrun{
#'   library(randomForest)
#'
#'   rf_fit <- function(x, y) {
#'     randomForest(x, y, ntree = 200)
#'   }
#'
#'   rf_predict <- function(object, newdata) {
#'     stats::predict(object, newdata, type = "prob")[, 2]
#'   }
#'
#'   rf <- base_estimator(rf_fit, rf_predict)
#' }
#'
#' @export
base_estimator <- function(fit_fn, predict_fn = stats::predict) {
  function(x, y) {
    m <- fit_fn(x, y)
    structure(list(est = m),
              class = "base_estimator",
              predict_fn = predict_fn)
  }
}

#' @export
predict.base_estimator <- function(object, newdata, ...) {
  fn <- attr(object, "predict_fn")
  fn(object$est, newdata)
}

rf_predict <- function(m, newdata) {
  stats::predict(m, newdata, type = "prob")[, 2]
}

rf_fit <- function(x, y) {
  # y must be a factor to make rf fit a classification model
  y <- factor(y)
  randomForest::randomForest(x, y, ntree = 200)
}

#' @export
random_forest <- base_estimator(fit_fn = rf_fit, predict_fn = rf_predict)
