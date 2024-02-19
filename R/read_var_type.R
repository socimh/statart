read_var_type <- function(var) {
  stat_tb <- tibble::tibble(
    type = vctrs::vec_ptype_full(var) %>%
      stringr::str_extract("^\\w+"),
    unit = s_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = dplyr::n_distinct(var, na.rm = TRUE)
  )
  return(stat_tb)
}
