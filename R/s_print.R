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
    n = 5, width = NULL) {
  check_positive_int(n)
  check_positive_int(width)

  .data_tb <- tibble::as_tibble(.data)

  if (nrow(.data_tb) <= 2 * n + 1) {
    print(.data_tb, n = nrow(.data_tb), width = width)
  } else {
    if (tibble::has_rownames(.data_tb) && !is.null(.data_tb)) {
      .data_tb <- .data_tb %>%
        tibble::rownames_to_column(".rowname")
    } else if (!is.null(.data_tb)) {
      .data_tb <- .data_tb %>%
        tibble::rowid_to_column(".rowname")
    }

    paste0(
      "There are ", nrow(.data_tb), " rows in the dataset."
    ) %>%
      message()
    .data_tb %>%
      head(n) %>%
      dplyr::bind_rows(
        .data_tb %>% tail(n)
      ) %>%
      tibble::column_to_rownames(".rowname") %>%
    print(
      n = 2 * n,
      width = width,
      na.print = "<NA>"
    )
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
