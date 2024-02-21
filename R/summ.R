#' Summarise columns
#'
#' `r lifecycle::badge("experimental")`
#' Enhanced `summarise()`.
#' 
#' @param .data A data frame
#'
#' @return A tibble
#' @export
summ <- function(
    .data, ...,
    .by = NULL,
    .detail = FALSE,
    .stat = character(0)) {
  # Identify group variables
  group_vars <- extract_group_var(.data, .by)

  # Prepare data
  .data_summ <- keep_summ_vars(
    .data, ..., group_vars = group_vars
  )

  # Confirm variables
  check_numeric(.data_summ)
  check_missing(.data_summ)
  .data_num <- check_summarise(.data_summ)
  check_factor(.data_num)

  # Add group variables
  .data <- keep_useful_vars(.data, .data_num, group_vars)

  out <- summ_list(.data, group_vars, .detail, .stat)
  return(out)
  # .output <- summ_data_to_value(.data, vars, group_vars, .by, .stat, .detail, no_dot_by)

  # return(.output)
}
