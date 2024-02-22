#' Print a long tibble with its first and last rows
#'
#' @param .data A data frame, data frame extension
#' (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param n A positive integer. The number of rows to print.
#' @param width A positive integer. The width of the printed tibble.
#' @param .append If TRUE, will append the list into a tibble.
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
    class_tbl(.data, "head_tail", n) %>%
      print(width = width)
  }

  message(paste0(
    "Note: ",
    nrow(.data_tb) - 2 * n,
    " rows in the middle are hidden."
  ))

  return(invisible(.data))
}

# Helper function:
check_positive_int <- function(x) {
  not_positive_int <- !is.numeric(x) || any(x <= 0) || any(!x %% 1 == 0)
  if (not_positive_int && !is.null(x)) {
    stop(paste(x, "must be a positive integer"))
  }
}

class_tbl <- function(.data, class, n) {
  vctrs::new_data_frame(
    dplyr::bind_rows(
      .data %>% head(n),
      .data %>% tail(n)
    ),
    nrow = nrow(.data),
    rows = c(
      seq_len(n),
      seq(nrow(.data) - n + 1, nrow(.data))
    ),
    class = c(class, "tbl")
  )
}

tbl_sum.head_tail <- function(x, ...) {
  c("A tibble" = paste0(
    attr(x, "nrow"),
    " × ",
    ncol(x)
  ))
}

ctl_new_rowid_pillar.head_tail <- function(controller, x, width, ...) {
  out <- NextMethod()
  rowid <- attr(controller, "rows")
  width <- max(nchar(as.character(rowid)))
  list(
    title = out$title,
    type = out$type,
    data = pillar::new_pillar_shaft(
      list(row_ids = rowid),
      width = width,
      class = "pillar_rif_shaft"
    ) %>%
      pillar::pillar_component()
  ) %>%
    pillar::new_pillar(width = width)
}
