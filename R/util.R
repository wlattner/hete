select_control <- function(x, tmt) {
  select_status(x, tmt, status = 0)
}

select_treatment <- function(x, tmt) {
  select_status(x, tmt, status = 1)
}

select_status <- function(x, tmt, status) {
  if (is.null(dim(x))) {
    return(x[tmt == status])
  }

  return(x[tmt == status, ])
}

`%||%` <- function(a, b) {
  if (is.null(a)) {
    return(b)
  }

  return(a)
}

check_tmt <- function(tmt) {
  if (is.numeric(tmt)) {
    if (!setequal(tmt, c(1, 0))) {
      stop("tmt must be a vector of 1/0 of a factor with 2 levels")
    }

    return(tmt)
  }

  tmt <- forcats::fct_drop(tmt)
  if (length(levels(tmt)) != 2) {
    stop("tmt must be a vector of 1/0 or a factor with 2 levels")
  }
  # assume the reference level is control
  ref_level <- levels(tmt)[1]
  tmt <- ifelse(tmt == ref_level, 0, 1)
  return(tmt)
}

check_y <- function(y) {
  if (is.numeric(y)) {
    if (setequal(y, c(0, 1))) {
      message("y only has 1/0 to model a binary outcome, y must be a factor")
    }

    # don't need to do anything in this case
    return(y)
  }

  # probably a factor
  y <- forcats::fct_drop(y)
  if (length(levels(y)) != 2) {
    stop("y must be numeric or a factor with 2 levels")
  }
  return(y)
}

as_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  ref_level <- levels(x)[1]
  x <- ifelse(x == ref_level, 0, 1)
  return(x)
}

extract_model_terms <- function(m, x) {
  if (!is.null(m$model_terms) && is.data.frame(x)) {
    x <- stats::model.matrix(m$model_terms, x)
  }

  x
}
