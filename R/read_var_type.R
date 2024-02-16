read_var_type <- function(var) {
  stat_tb <- tibble(
    type = vctrs::vec_ptype_full(var) %>%
      str_extract("^\\w+"),
    unit = get_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = n_distinct(var, na.rm = TRUE)
  )
  return(stat_tb)
}