#' Split Estimators for Control and Treatment
#'
#' @inheritParams hete_single
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
hete_split.default <- function(x, y, tmt, est, ...) {
  hete_split_impl(x, y, tmt, est)
}

#' @export
#' @rdname hete_split
hete_split.formula <- function(x, data, est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_split_impl(dat$x, dat$y, dat$tmt, est, model_terms = dat$model_terms)
}

hete_split_impl <- function(x, y, tmt, est, model_terms = NULL) {
  m_1 <- est(x[tmt == 1, ], y[tmt == 1])
  m_0 <- est(x[tmt == 0, ], y[tmt == 0])

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
