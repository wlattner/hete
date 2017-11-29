#' Out of Sample Predictions Using Cross Valiation
#'
#' @inheritParams hete_single
#' @param model a \code{hete_model} to use for making predictions.
#'   are passed to the \code{model} fitting function.
#' @param folds the number of folds to use for cros validation.
#' @param \dots extra parameters passed to \code{model}.
#'
#' @export
cv_predict <- function(x, model, folds, ...) {
  UseMethod("cv_predict")
}

#' @rdname cv_predict
#' @export
cv_predict.default <- function(x, y, tmt, model, folds = 3, ...) {
  cv_predict_impl(x, y, tmt, model = model, folds = folds, ...)
}

#' @rdname cv_predict
#' @export
cv_predict.formula <- function(x, data, model, folds = 3, ...) {
  dat <- parse_hete_formula(x, data = data)
  cv_predict_impl(dat$x, dat$y, dat$tmt, model = model, folds = folds, ...)
}

cv_predict_impl <- function(x, y, tmt, model, folds = 3, ...) {
  fold_assign <- sample(seq_len(folds), size = nrow(x), replace = TRUE)
  fold_ind <- split(seq_len(nrow(x)), fold_assign)

  model_fn <- purrr::partial(model, ...)

  # TODO: parallel
  df <- purrr::map_dfr(fold_ind, fit_predict, x = x, y = y, tmt = tmt, model = model_fn)
  df[order(df$ind), 'predicted_te']
}

fit_predict <- function(test_ind, x, y, tmt, model) {
  train_mask <- logical(nrow(x))
  train_mask[-test_ind] <- TRUE

  x_tr <- x[train_mask, ]
  y_tr <- y[train_mask]
  tmt_tr <- tmt[train_mask]

  m <- model(x_tr, y_tr, tmt_tr)
  p <- predict(m, x[!train_mask, ], y[!train_mask], tmt[!train_mask])

  data.frame(ind = test_ind, predicted_te = p)
}
