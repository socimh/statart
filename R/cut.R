#' Cut a numeric vector into intervals.
#'
#' @param x A numeric vector.
#' @param n A positive integer. The number of intervals.
#' @inheritParams base::cut
#'
#' @return Print the selected rows of the data.
#' Return the input data frame invisibly.
#'
#' @name cut
#' @examples
#' cut_quantile(1:20)
#'
#' cut_length(1:20)
#'
#' cut_breaks(1:20, breaks = c(5, 15))

#' @rdname cut
#' @export
cut_quantile <- function(x, n = 4, labels = NULL) {
  breaks <- quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)
  cut_breaks(x, breaks = breaks, labels = labels) %>%
    return()
}

#' @rdname cut
#' @export
cut_length <- function(x, n = 4, labels = NULL) {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
  cut_breaks(x, breaks = breaks, labels = labels) %>%
    return()
}

#' @rdname cut
#' @export
cut_breaks <- function(x, breaks = NULL, labels = NULL, right = TRUE) {
  if (is.null(breaks)) {
    breaks <- median(x, na.rm = TRUE)
  }
  min <- min(x, na.rm = TRUE)
  max <- max(x, na.rm = TRUE)
  if (min == max) {
    stop("Cannot cut a vector with only one unique value.")
  }

  breaks <- c(min, breaks, max) %>%
    unique()

  cut(x,
    breaks = breaks,
    include.lowest = TRUE,
    right = right,
    labels = labels,
    ordered_result = TRUE
  ) %>%
    return()
}
