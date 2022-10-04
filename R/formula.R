parse_hete_formula <- function(f, data) {
  lhs <- rlang::f_lhs(f)
  rhs <- rlang::f_rhs(f)
  # verify we have correct form
  if (rlang::call_name(rhs) != "|") {
    stop("formula must be in the form: y ~ x1 + x2 + ... + xn | tmt")
  }
  # The rhs of the formula is parsed as a call to the function `|`, the first
  # arg is the model formula, the second is the treatment indicator.
  rhs_args <- rlang::call_args(rhs)
  # We're adding the response (`lhs`) back in so it's not accidentally included
  # below when we build the terms object. If the caller used `.`, the call to
  # terms will pull the response back into the model since it's in `data`.
  model_formula <- rlang::new_formula(lhs, rhs_args[[1L]])
  tmt_expr <- rhs_args[[2L]]
  model_terms <- stats::terms(model_formula, data = data)
  # Remove the treatment indicator in case the caller included it via `.`, also
  # drop the intercept term.
  model_terms <- stats::update(model_terms, paste0(". ~ . -1 -", tmt_expr))

  x <- stats::model.matrix(model_terms, data = data)
  y <- eval(lhs, envir = data)
  tmt <- eval(tmt_expr, envir = data)

  list(x = x, y = y, tmt = tmt, model_terms = model_terms)
}
