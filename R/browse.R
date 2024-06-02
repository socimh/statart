#' Browse columns like Stata's `browse` command
#'
#' @param .data A data frame
#' @param ... <[`tidy-select`][dplyr_tidy_select]> or
#' <[`data-masking`][args_data_masking]> Variables to browse.
#' *This argument can be omitted*.
#'
#' @return A character vector of column names.
#' @export
#'
#' @examples
#' browse(starwars)
#'
#' browse(starwars, name:mass)
#'
#' browse(starwars, s_match("*color"))
browse <- function(.data, ...) {
  if (missing(...)) {
    tibble::view(.data)
  } else {
    .data %>%
      s_select(...) %>%
      tibble::view()
  }
}