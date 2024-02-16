get_type_abbr <- function(.x) {
  type <- vctrs::vec_ptype_abbr(.x)
  if (vctrs::vec_ptype_full(.x) %>%
    str_detect("^labelled")) {
    type <- "lbl"
  }
  return(type)
}
