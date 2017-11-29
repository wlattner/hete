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
