#' List column names by tidyselect
#'
#' @param .data A data frame
#' @param ... <[`tidy-select`][dplyr_tidy_select]> Variables to select.
#'
#' @return A character vector of column names.
#' @name names
#' @seealso [variables()], [codebook()], [browse()]
#'
#' @examples
#' ds(starwars) # equivalent to names(starwars)
#' 
#' ds(starwars, homeworld:starships)
#' 
#' ds_as_column(starwars, ends_with("color"))
#' 
#' ds_as_column(starwars, where(is.character))
#' 
#' 
#' names_as_column(starwars)
#' 
#' names_as_column(starwars, ends_with("color"))
#' 
#' names_as_column(starwars, where(is.character))


#' @rdname names
#' @export
ds <- function(.data, ...) {
  if (missing(...)) {
    names(.data) %>%
      return()
  } else {
    .data %>%
      dplyr::select(...) %>%
      names() %>%
      return()
  }
}

#' @rdname names
#' @export
ds_as_column <- function(.data, ...) {
  names_as_column(.data, ...)
}

#' @rdname names
#' @export
names_as_column <- function(.data, ...) {
  ds(.data, ...) %>%
    tibble::as_tibble_col("name")
}