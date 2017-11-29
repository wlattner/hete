hete_model <- function(x, y, tmt, model_terms, ..., subclass) {
  attrs <- rlang::dots_list(...)
  attrs[['train_data']] <- list(x = x, y = y, tmt = tmt)
  structure(attrs, class = c(subclass, "hete_model"))
}

#' @export
print.hete_model <- function(x, ...) {
  cat("A hete_model\n")
  cat("Method: ", meta_learner_name(x), "\n")
}

meta_learner_name <- function(x) {
  class(x)[1]
}
