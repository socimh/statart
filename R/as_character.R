#' Enhance the performance of as.character()
#'
#' @param .x A vector.
#' @return A character vector with the same length as `.x`.
#' @export
#'
#' @examples
#' lifeexp %>%
#'   select(region)
#' lifeexp %>%
#'   mutate(
#'     region_chr = as_character(region),
#'     .keep = "used"
#'   )
as_character <- function(.x) {
  type <- get_type_abbr(.x)
  if (type == "lbl") {
    vec <- .x %>%
      haven::as_factor() %>%
      as.character()
  } else {
    vec <- .x %>%
      as.character()
  }
  return(vec)
}
