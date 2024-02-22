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
#'     region_fct = as_factor(region),
#'     .keep = "used"
#'   )

#' @export
#' @name as_type
as_character <- function(.x) {
  type <- s_type(.x, .abbr = TRUE)
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

#' @export
#' @name as_type
as_numeric <- function(var) {
  if (s_type(var, .abbr = TRUE) %in%
    c("units", "drtn", "time", "fct", "ord")) {
    var <- as.numeric(var)
  } else if (s_type(var, .abbr = TRUE) == "lbl") {
    library(haven)
    var <- as.numeric(var)
  } else if (s_type_summable(var)) {
    var <- var
  } else {
    var <- NA_real_
  }
  return(var)
}
