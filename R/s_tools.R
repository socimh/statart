#' The clean type of an object.
#'
#' @param .x An object.
#' @return A character.
#' @export
#'
#' @examples
#' s_type(1:10)
#' s_type(letters) 
#' @export
s_type <- function(.x, .full = FALSE) {
  type <- .x %>%
    pillar::type_sum() %>%
    stringr::str_extract("^(\\w|\\+)+")

    if (vctrs::vec_ptype_abbr(.x) == "units") {
      type <- "units"
    }

  if (.full) {
    type <- dplyr::case_when(
      type == "int" ~ "integer",
      type == "dbl" ~ "double",
      type == "chr" ~ "character",
      type == "lgl" ~ "logical",
      type == "cpl" ~ "complex",
      type == "fct" ~ "factor",
      type == "dttm" ~ "datetime",
      type == "drtn" ~ "duration",
      type == "df" ~ "dataframe",
      type == "fn" ~ "function",
      type == "env" ~ "environment",
      type == "sym" ~ "symbol",
      str_detect(type, "lbl") ~ "labelled",
      TRUE ~ type
    )
  }

  return(type)
}

s_unit <- function(x) {
  unit <- NA_character_

  if (s_type(x) %in% c("units", "drtn", "time")) {
    unit <- units(x) %>% as.character()
  }

  return(unit)
}