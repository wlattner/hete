#' @export
uplift <- function(y, tmt, pred_te, bins = 10) {

  # boolean if the user passed 1/0
  tmt <- as.logical(tmt)

  if (is.factor(y)) {
    # TODO: should we warn user we're doing this?
    ref_level <- levels(y)[1]
    y <- ifelse(y == ref_level, 0, 1)
  }

  ate <- mean(y[tmt]) - mean(y[!tmt])
  frac <- seq(0, 1, 1 / bins)
  random_lift <- ate * frac

  # we want to order the scores from highest to lowest
  qts <- stats::quantile(pred_te, probs = rev(frac))
  model_lift <- purrr::map_dbl(qts, model_lift, y = y, tmt = tmt, pred_te = pred_te)
  # the first one must be 0
  model_lift[1] <- 0

  df <- tibble::tibble(frac = frac, model = model_lift, random = random_lift)
  structure(
    list(
      uplift_curve = df,
      q = qini(frac, random_lift, model_lift),
      ate_observed = ate,
      ate_predicted = mean(pred_te)
    ),
    class = "hete_uplift")
}

model_lift <- function(min_val, y, tmt, pred_te) {
  y_ <- y[pred_te >= min_val]
  tmt_ <- tmt[pred_te >= min_val]
  mean(y_[tmt_]) - mean(y_[!tmt_])
}

#' @export
plot.hete_uplift <- function(x, ...) {
  df_plot <- tidyr::gather(x$uplift_curve, key = "measure", value = "value", -c("frac"))
  p <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x = "frac", y = "value", color = "measure")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(label = scales::percent) +
    ggplot2::xlab("Population Fraction") +
    ggplot2::ylab("Cumulative Lift") +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL)) +
    ggplot2::theme_bw()
  p
}

qini <- function(target_frac, random, model) {
  random_auc <- trapz(target_frac, random)
  model_auc <- trapz(target_frac, model)

  model_auc - random_auc
}

trapz <- function(x, y) {
  delta <- diff(x)
  y1 <- y[-1]
  y2 <- y[-length(y)]

  sum(((y1 + y2) / 2) * delta)
}

#' @importFrom rlang .data
plot_quantiles <- function(y, tmt, pred_te, bins = 10) {
  df <- tibble::tibble(y = y, tmt = tmt, pred_te = pred_te)
  # TODO: NSE fix here
  df <- dplyr::mutate(df,
                      tmt_quantile = factor(dplyr::ntile(.data$pred_te, bins)),
                      ctl = ifelse(.data$tmt == 0, 1, 0))

  # TODO: group_by_ is deprecated, figure out the NSE magic to make group_by
  # take a string as the column name.
  qt <- dplyr::group_by_(df, "tmt_quantile")
  qt <- dplyr::mutate(qt,
                      y_0 = ifelse(.data$tmt == 0, .data$y, 0),
                      y_1 = ifelse(.data$tmt == 1, .data$y, 0))
  qt <- dplyr::summarize(qt,
                         Predicted = mean(.data$pred_te),
                         ctl_mean = sum(.data$y_0) / sum(.data$ctl),
                         tmt_mean = sum(.data$y_1) / sum(.data$tmt),
                         Actual = .data$tmt_mean - .data$ctl_mean)
  qt <- tidyr::gather(qt, key = "series", value = "tmt_effect", "Predicted", "Actual")

  p <- ggplot2::ggplot(qt, ggplot2::aes_string(x = "tmt_quantile", y = "tmt_effect", fill = "series")) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::xlab("Predicted Treatment Effect Quantile") +
    ggplot2::ylab("Treatment Effect") +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +
    ggplot2::theme_bw()
  p
}
