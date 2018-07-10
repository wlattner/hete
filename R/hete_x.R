#' X-Learner Meta-Estimator
#'
#' @inheritParams hete_single
#'
#' @param est,tmt_est,ctl_est,te_tmt_est,te_ctl_est an estimator to use for
#'   modeling the treatment effect. This must be a function which takes two
#'   arguments, \code{x} and \code{y} and returns an object which has an
#'   implementation of \code{\link[stats]{predict}}. The \code{predict}
#'   implementation must return a single vector with the estimated outcome or
#'   probability of success in the case of binary outcomes. By default the same
#'   estimator, \code{est} is used for all four models in the meta-estimator.
#'   You may optionally select a different estimator for one or more of these
#'   models. \code{tmt_est} and \code{ctl_est} are used to estimate the response
#'   in the treatment and control groups respectively. \code{te_tmt_est} and
#'   \code{te_ctl_est} are used to estimate the treatment effect in the
#'   treatment and control groups.
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
#' uc <- uplift(df$voted, df$treatment, p)
#' plot(uc)
#'
#' }
#' @references Künzel, S., Sekhon, J., Bickel, P., & Yu, B. (2017). Meta-learners for Estimating Heterogeneous Treatment Effects using Machine Learning. arXiv preprint arXiv:1706.03461.
hete_x <- function(x, ...) {
  UseMethod("hete_x")
}

#' @export
#' @rdname hete_x
hete_x.default <- function(x, y, tmt, est, tmt_est = est, ctl_est = est,
                           te_tmt_est = est, te_ctl_est = est, ...) {
  hete_x_impl(x, y, tmt, tmt_est, ctl_est, te_tmt_est, te_ctl_est)
}

#' @export
#' @rdname hete_x
hete_x.formula <- function(x, data, est, tmt_est = est, ctl_est = est,
                           te_tmt_est = est, te_ctl_est = est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_x_impl(dat$x, dat$y, dat$tmt, tmt_est, ctl_est, te_tmt_est, te_ctl_est,
              model_terms = dat$model_terms)
}

hete_x_impl <- function(x, y, tmt, tmt_est, ctl_est, te_tmt_est, te_ctl_est,
                        model_terms = NULL) {
  y <- check_y(y)
  tmt <- check_tmt(tmt)

  x_0 <- select_control(x, tmt)
  y_0 <- select_control(y, tmt)
  u_c <- ctl_est(x_0, y_0)

  x_1 <- select_treatment(x, tmt)
  y_1 <- select_treatment(y, tmt)
  u_t <- tmt_est(x_1, y_1)

  d_0 <- stats::predict(u_t, x_0) - as_numeric(y_0)
  d_1 <- as_numeric(y_1) - stats::predict(u_c, x_1)

  t_0 <- te_ctl_est(x_0, d_0)
  t_1 <- te_tmt_est(x_1, d_1)

  hete_model(x, y, tmt, t_0 = t_0, t_1 = t_1, g = mean(tmt),
             model_terms = model_terms, subclass = "hete_x")
}

#' @export
predict.hete_x <- function(object, newdata, ...) {
  newdata <- extract_model_terms(object, newdata)

  t_hat_0 <- stats::predict(object$t_0, newdata)
  t_hat_1 <- stats::predict(object$t_1, newdata)

  # TODO: verify we want to use the propensity score here
  object$g * t_hat_0 + (1 - object$g) * t_hat_1
}
