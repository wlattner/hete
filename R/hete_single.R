#' Single Estimator
#'
#' @param x a \code{data.frame} or matrix with predictor variables measured prior to
#'   treatment or unaffected by treatment. Alternatively, this can be a model
#'   formula: \code{outcome ~ x1 + x2 | treatment}
#' @param data a \code{data.frame} containing the variables specified in
#'   \code{x} when using a formula to specify the model.
#' @param y a vector of outcomes.
#' @param tmt a vector indicating which units received treatment.
#' @param est an estimator to use for modeling the treatment effect. This must
#'   be a function which takes two arguments, \code{x} and \code{y} and returns
#'   an object which has an implementation of \code{\link[stats]{predict}}. The
#'   \code{predict} implementation must return a single vector with the
#'   estimated outcome or probability of success in the case of binary outcomes.
#' @param \dots ignored.
#'
#' @return A \code{hete_single} object.
#'
#' @export
#' @examples \dontrun{
#' library(tidyverse)
#' data(gotv)
#'
#' df <- gotv %>%
#'   filter(treatment %in% c("Control", "Neighbors")) %>%
#'   mutate(treatment = ifelse(treatment == "Control", 0, 1))
#'
#' m <- hete_single(voted ~ . | treatment, data = df, est = random_forest)
#' p <- predict(m, df)
#'
#' uc <- uplift(df$treatment, df$voted, p)
#' plot(uc)
#'
#' }
hete_single <- function(x, ...) {
  UseMethod("hete_single")
}

#' @export
#' @rdname hete_single
hete_single.default <- function(x, y, tmt, est, ...) {
  hete_single_impl(x, y, tmt, est)
}

#' @export
#' @rdname hete_single
hete_single.formula <- function(x, data, est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_single_impl(dat$x, dat$y, dat$tmt, est, model_terms = dat$model_terms)
}

hete_single_impl <- function(x, y, tmt, est, model_terms = NULL) {
  y <- check_y(y)
  tmt <- check_tmt(tmt)

  m <- est(cbind(x, tmt), y)

  hete_model(x, y, tmt, model = m, model_terms = model_terms,
             subclass = "hete_single")
}

#' @param object A \code{hete_single} model.
#' @param newdata A \code{data.frame} or matrix containing data to make
#'   predictions for.
#' @export
#' @rdname hete_single
predict.hete_single <- function(object, newdata, ...) {
  if (!is.null(object$model_terms)) {
    # The model was fitted using a formula. Build the design matrix for
    # prediction.
    newdata <- stats::model.matrix(object$model_terms, newdata)
  }

  y_1 <- stats::predict(object$model, cbind(newdata, tmt = 1))
  y_0 <- stats::predict(object$model, cbind(newdata, tmt = 0))

  y_1 - y_0
}
