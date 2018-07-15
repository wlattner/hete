hete_model <- function(x, y, tmt, model_terms, ..., subclass) {
  attrs <- rlang::dots_list(...)
  attrs$model_terms <- model_terms
  attrs$train_data <- list(x = x, y = y, tmt = tmt)

  m <- structure(attrs, class = c(subclass, "hete_model"))
  m <- add_predictions(m)
  m <- add_observed_ate(m)
  m <- add_observation_counts(m)

  m
}

add_observation_counts <- function(obj) {
  tmt <- obj$train_data$tmt
  obj$n_observations <- length(tmt)
  obj$n_treatment <- sum(tmt)
  obj$n_control <- obj$n_observations - obj$n_treatment
  obj
}

add_predictions <- function(obj) {
  x <- obj$train_data$x
  p <- stats::predict(obj, x)
  obj$pred_te <- p
  obj$predicted_ate <- mean(p)
  obj
}

as_numeric_outcome <- function(y) {
  if (is.factor(y)) {
    ref_level <- levels(y)[1]
    y <- ifelse(y == ref_level, 0, 1)
  }

  y
}

add_observed_ate <- function(obj) {
  y <- as_numeric_outcome(obj$train_data$y)
  tmt <- obj$train_data$tmt

  obj$observed_ate <- mean(y[tmt == 1]) - mean(y[tmt == 0])
  obj
}

#' @export
print.hete_model <- function(x, ...) {
  cat("A hete_model\n")
  cat("Method:", meta_learner_name(x), "\n")
  cat("\n")
  cat("Number of Observations:", x$n_observations, "\n")
  cat("             Treatment:", x$n_treatment, "\n")
  cat("               Control:", x$n_control, "\n")
  cat("\n")
  cat(" Observed ATE:", x$observed_ate, "\n")
  cat("Predicted ATE:", mean(x$pred_te), "\n")
}

#' @export
plot.hete_model <- function(x, y, what = c("uplift", "quantile"), ...) {
  what <- match.arg(what)
  switch(what,
    uplift = graphics::plot(uplift(x$train_data$y, x$train_data$tmt, x$pred_te)),
    quantile = plot_quantiles(x$train_data$y, x$train_data$tmt, x$pred_te)
  )
}

meta_learner_name <- function(x) {
  class(x)[1]
}
