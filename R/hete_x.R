#' X-Learner Meta-Estimator
#'
#' @inheritParams hete_single
#'
#' @return A \code{hete_x} object.
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
#' m <- hete_x(voted ~ . | treatment, data = df, est = random_forest)
#' p <- predict(m, df)
#'
#' uc <- uplift(df$treatment, df$voted, p)
#' plot(uc)
#'
#' }
#' @references Künzel, S., Sekhon, J., Bickel, P., & Yu, B. (2017). Meta-learners for Estimating Heterogeneous Treatment Effects using Machine Learning. arXiv preprint arXiv:1706.03461.
hete_x <- function(x, ...) {
  UseMethod("hete_x")
}

#' @export
#' @rdname hete_x
hete_x.default <- function(x, y, tmt, est, ...) {
  hete_x_impl(x, y, tmt, est)
}

#' @export
#' @rdname hete_x
hete_x.formula <- function(x, data, est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_x_impl(dat$x, dat$y, dat$tmt, est, model_terms = dat$model_terms)
}

hete_x_impl <- function(x, y, tmt, est, model_terms = NULL) {
  # TODO: Users may want to provide two different estimators for this
  # meta learner.
  x_0 <- select_control(x, tmt)
  y_0 <- select_control(y, tmt)
  u_c <- est(x_0, y_0)

  x_1 <- select_treatment(x, tmt)
  y_1 <- select_treatment(y, tmt)
  u_t <- est(x_1, y_1)

  d_0 <- stats::predict(u_t, x_0) - y_0
  d_1 <- y_1 - stats::predict(u_c, x_1)

  t_0 <- est(d_0, x_0)
  t_1 <- est(d_1, x_1)

  hete_model(x, y, tmt, t_0 = t_0, t_1 = t_1, g = mean(tmt),
             model_terms = model_terms, subclass = "hete_x")
}

#' @export
predict.hete_x <- function(object, newdata, ...) {
  if (!is.null(object$model_terms)) {
    newdata <- stats::model.matrix(object$model_terms, newdata)
  }

  t_hat_0 <- stats::predict(object$t_0, newdata)
  t_hat_1 <- stats::predict(object$t_1, newdata)

  # TODO: verify we want to use the propensity score here
  object$g * t_hat_0 + (1 - object$g) * t_hat_1
}
