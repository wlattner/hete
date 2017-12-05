#' Transformed Outcome Estimator
#'
#' @inheritParams hete_single
#'
#' @return A \code{hete_tot} object.
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
#' m <- hete_tot(voted ~ . | treatment, data = df, est = random_forest)
#' p <- predict(m, df)
#'
#' uc <- uplift(df$treatment, df$voted, p)
#' plot(uc)
#'
#' }
hete_tot <- function(x, ...) {
  UseMethod("hete_tot")
}

#' @export
#' @rdname hete_tot
hete_tot.default <- function(x, y, tmt, est, ...) {
  hete_tot_impl(x, y, tmt, est)
}

#' @export
#' @rdname hete_tot
hete_tot.formula <- function(x, data, est, ...) {
  dat <- parse_hete_formula(x, data = data)
  hete_tot_impl(dat$x, dat$y, dat$tmt, est, model_terms = dat$model_terms)
}

hete_tot_impl <- function(x, y, tmt, est, model_terms) {
  y <- check_y(y)
  tmt <- check_tmt(tmt)

  e <- mean(tmt)
  z <- y * (tmt - e) / (e * (1 - e))

  m <- est(x, z)

  hete_model(x, y, tmt, model = m, model_terms = model_terms,
             subclass = "hete_tot")
}

#' @export
predict.hete_tot <- function(object, newdata, ...) {
  if (!is.null(object$model_terms)) {
    newdata <- stats::model.matrix(object$model_terms, newdata)
  }

  stats::predict(object$model, newdata)
}
