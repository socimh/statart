#' Get the abbreviated type of a vector.
#'
#' @param .x A vector.
#' @returns A character.
#' @examples
#' get_type_abbr(1:10)
#' get_type_abbr("abc")
get_type_abbr <- function(.x) {
  type <- vctrs::vec_ptype_abbr(.x)
  if (vctrs::vec_ptype_full(.x) %>%
    stringr::str_detect("^labelled")) {
    type <- "lbl"
  }
  return(type)
}
