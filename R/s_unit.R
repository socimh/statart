s_unit <- function(x) {
  unit <- NA_character_

  if (get_type_abbr(x) %in% c("units", "drtn", "time")) {
    unit <- units(x) %>% as.character()
  }

  return(unit)
}
