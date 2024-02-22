#' The clean type of an object.
#'
#' @param .x An object.
#' @param .full A logical. If `TRUE`, the full name of the type is returned.
#' @return A character.
#' @export
#'
#' @examples
#' s_type(1:10)
#' s_type(letters) 
#' @export
s_type <- function(.x, .abbr = FALSE) {
  type <- .x %>%
    pillar::type_sum() %>%
    stringr::str_extract("^(\\w|\\+)+")

    if (vctrs::vec_ptype_abbr(.x) == "units") {
      type <- "units"
    }
    if (stringr::str_detect(type, "lbl")) {
      type <- "lbl"
    }

  if (!.abbr) {
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
      type == "lbl" ~ "labelled",
      TRUE ~ type
    )
  }

  return(type)
}

s_unit <- function(x) {
  unit <- NA_character_

  if (s_type(x, .abbr = TRUE) %in% c("units", "drtn", "time")) {
    unit <- units(x) %>% as.character()
  }

  return(unit)
}