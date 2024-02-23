#' Better type conversion
#'
#' It improves the performance of converting
#' several types of variables.
#'
#' @name as_type
#' @param .x A vector or matrix.
#'
#' @examples
#' lifeexp %>%
#'   dplyr::mutate(
#'     region_chr = as_character(region),
#'     region_num = as_numeric(region),
#'     region_fct = haven::as_factor(region),
#'     .keep = "used"
#'   )

#' @export
#' @rdname as_type
as_character <- function(.x) {
  type <- s_type(.x, .abbr = TRUE)
  if (type %>% stringr::str_detect("lbl")) {
    vec <- .x %>%
      haven::as_factor() %>%
      as.character()
  } else {
    vec <- .x %>%
      as.character()
  }
  return(vec)
}

#' @export
#' @rdname as_type
as_numeric <- function(.x) {
  if (s_type(.x, .abbr = TRUE) %in%
    c("units", "drtn", "time", "fct", "ord")) {
    .x <- as.numeric(.x)
  } else if (s_type(.x, .abbr = TRUE) %>%
    stringr::str_detect("lbl")) {
    .x <- unclass(.x) %>% as.numeric()
  } else if (!is_summable(.x)) {
    .x <- NA_real_
  }
  return(.x)
}
