#' List column names by tidyselect
#'
#' @param .data A data frame
#' @param ... <[`tidy-select`][dplyr_tidy_select]> Variables to select.
#'
#' @return A character vector of column names.
#' @export
#'
#' @examples
#' ds(starwars)
#' ds(starwars, homeworld:starships)
#' ds(starwars, ends_with("color"))
#' ds(starwars, where(is.character))
ds <- function(.data, ...) {
  .data %>%
    dplyr::select(...) %>%
    names()
}
