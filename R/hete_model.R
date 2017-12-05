hete_model <- function(x, y, tmt, model_terms, ..., subclass) {
  attrs <- rlang::dots_list(...)
  attrs[['train_data']] <- list(x = x, y = y, tmt = tmt)
  m <- structure(attrs, class = c(subclass, "hete_model"))
  add_predictions(m)
}

add_predictions <- function(obj) {
  p <- stats::predict(obj, obj$train_data$x)
  obj$pred_te <- p
  obj
}

#' @export
print.hete_model <- function(x, ...) {
  cat("A hete_model\n")
  cat("Method:", meta_learner_name(x), "\n")
  cat("Predicted ATE:", mean(x$pred_te), "\n")
}

#' @export
plot.hete_model <- function(x, y, what = c("uplift", "quantile"), ...) {
  what <- match.arg(what)
  switch(what,
    uplift = plot(uplift(x$train_data$y, x$train_data$tmt, x$pred_te)),
    quantile = plot_quantiles(x$train_data$y, x$train_data$tmt, x$pred_te)
  )
}

meta_learner_name <- function(x) {
  class(x)[1]
}
