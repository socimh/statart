#' Enhance the performance of as.character()
#'
#' @param .x A vector.
#' @return A character vector with the same length as `.x`.
#' @export
#'
#' @examples
#' lifeexp %>%
#'   dplyr::select(region)
#' lifeexp %>%
#'   dplyr::mutate(
#'     region_chr = as_character(region),
#'     .keep = "used"
#'   )
as_character <- function(.x) {
  type <- s_type(.x)
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
