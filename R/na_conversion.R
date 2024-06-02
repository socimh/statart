#' NA Conversion
#'
#' Convert NAs easily.
#'
#' @description
#' * `na_if_value()` converts a value to NA. The same as `dplyr::na_if()`.
#' * `value_if_na()` converts NA to a value. The same as `tidyr::replace_na()`.
#'
#'
#' @name na_conversion
#'
#' @param .x A vector.
#' @param .value A value to convert to NA.
#'
#' @examples
#' x <- c(1, 2, NA, 4)
#' na_if_value(x, 2)
#' value_if_na(x, 3)

#' @rdname na_conversion
#' @export
na_if_value <- function(.x, .value) {
  dplyr::na_if(.x, .value)
}

#' @rdname na_conversion
#' @export
value_if_na <- function(.x, .value) {
  tidyr::replace_na(.x, .value)
}
