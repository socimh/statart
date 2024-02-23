#' Print a long tibble with its first and last rows
#'
#' @param .data A data frame, data frame extension
#' (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param n A positive integer. The number of rows to show in
#' the head and in the tail.
#' @param width A positive integer. The width of the printed tibble.
#' @inheritParams tibble::print.tbl
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
    ...,
    max_extra_cols = NULL,
    max_footer_lines = NULL) {
  check_positive_int(n)
  check_positive_int(width)

  .data_tb <- tibble::as_tibble(.data)

  print_max <- max(2 * n + 1, 20)

  if (nrow(.data_tb) <= print_max) {
    print(.data_tb, n = nrow(.data_tb), width = width)
  } else {
    class_tbl(.data, "head_tail", n) %>%
      print(n = Inf, width = width)
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

#' @importFrom pillar tbl_sum
#' @export
tbl_sum.head_tail <- function(x) {
  c("A tibble" = paste0(
    attr(x, "nrow"),
    " \u00d7 ",
    ncol(x)
  ))
}

#' @importFrom pillar ctl_new_rowid_pillar
#' @export
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

#' @importFrom pillar tbl_format_footer
#' @export
tbl_format_footer.head_tail <- function(x, ...) {
  default_footer <- NextMethod()

  exclamation <- "\u2139"
  hidden_rows <- attr(x, "nrow") - length(attr(x, "rows"))
  rows_footer <- paste0(
    "# ", exclamation, " ", hidden_rows, " more rows in the middle"
  ) %>%
    pillar::style_subtle()
  tip_footer <- paste0(
    "# ", exclamation, " Use `s_print(n = ...)` to see more rows"
  ) %>%
    pillar::style_subtle()

  c(rows_footer, default_footer, tip_footer)
}
