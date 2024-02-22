#' Print a long tibble with its first and last rows
#'
#' @param .data A data frame, data frame extension
#' (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param n A positive integer. The number of rows to print.
#' @param width A positive integer. The width of the printed tibble.
#' @param .head_row If TRUE, will add the first row in the output.
#' Postive integers are also accepted.
#' @param .tail_row If TRUE, will add the last row in the output.
#' Postive integers are also accepted.
#'
#' @return Print the first and last rows of the data.
#' Return the input data frame invisibly.
#'
#' @examples
#' s_print(starwars)
#' fre1(starwars, .append = TRUE) %>%
#'   s_print()
#'
#' @export
s_print <- function(
    .data,
    n = 5, width = NULL,
    .head_row = FALSE, .tail_row = FALSE) {
  check_positive_int(n)
  check_positive_int(width)

  .data_tb <- tibble::as_tibble(.data)

  head_n <- n + .head_row
  tail_n <- n + .tail_row
  if (nrow(.data_tb) <= head_n + tail_n + 1) {
    print(.data_tb, n = nrow(.data_tb), width = width)
  } else {
    paste0(
      "There are ", nrow(.data_tb), " rows in the dataset."
    ) %>%
      message()
    paste(
      "# Top", head_n, "rows:"
    ) %>%
      message()
    # I tried to adjust the header of the tibble output,
    # but currently pillar does not support it within functions.
    print(
      .data_tb %>% head(head_n),
      n = head_n, width = width,
      max_footer_lines = 0
    )
    paste(
      "[......",
      nrow(.data_tb) - head_n - tail_n,
      "rows omitted ......]"
    ) %>%
      message()
    paste(
      "# Bottom", tail_n, "rows:"
    ) %>%
      message()
    print(.data_tb %>% tail(tail_n), n = tail_n, width = width)
  }
return(invisible(.data))
}

# Helper function:
check_positive_int <- function(x) {
  not_positive_int <- !is.numeric(x) || any(x <= 0) || any(!x %% 1 == 0)
  if (not_positive_int && !is.null(x)) {
    stop(paste(x, "must be a positive integer"))
  }
}