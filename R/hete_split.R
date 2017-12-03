#' Split Estimators for Control and Treatment
#'
#' @inheritParams hete_single
#' @param est,tmt_est,ctl_est an estimator to use for modeling the treatment
#'   effect. This must be a function which takes two arguments, \code{x} and
#'   \code{y} and returns an object which has an implementation of
#'   \code{\link[stats]{predict}}. The \code{predict} implementation must return
#'   a single vector with the estimated outcome or probability of success in the
#'   case of binary outcomes. By default the same estimator, \code{est} is used
#'   for both models in the meta-estimator. You may optionally select a
#'   different estimator for each models. \code{tmt_est} and \code{ctl_est} are
#'   used to estimate the response in the treatment and control groups
#'   respectively.
#'
#' @return A \code{hete_split} object.
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
#' m <- hete_split(voted ~ . | treatment, data = df, est = random_forest)
#' p <- predict(m, df)
#'
#' uc <- uplift(df$treatment, df$voted, p)
#' plot(uc)
#'
#' }
hete_split <- function(x, ...) {
  UseMethod("hete_split")
}

#' @export
#' @rdname hete_split
hete_split.default <- function(x, y, tmt, est, tmt_est = est, ctl_est = est, ...) {
  hete_split_impl(x, y, tmt, tmt_est, ctl_est)
}

#' @export
#' @rdname hete_split
hete_split.formula <- function(x, data, est, tmt_est = est, ctl_est = est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_split_impl(dat$x, dat$y, dat$tmt, tmt_est, ctl_est,
                  model_terms = dat$model_terms)
}

hete_split_impl <- function(x, y, tmt, tmt_est, ctl_est, model_terms = NULL) {
  m_1 <- tmt_est(x[tmt == 1, ], y[tmt == 1])
  m_0 <- ctl_est(x[tmt == 0, ], y[tmt == 0])

  hete_model(x, y, tmt, treatment_model = m_1, control_model = m_0,
             model_terms = model_terms, subclass = "hete_split")
}

#' @export
predict.hete_split <- function(object, newdata, ...) {
  if (!is.null(object$model_terms)) {
    newdata <- stats::model.matrix(object$model_terms, newdata)
  }

  y_1 <- stats::predict(object$treatment_model, newdata)
  y_0 <- stats::predict(object$control_model, newdata)

  y_1 - y_0
}
