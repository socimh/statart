#' Summarise columns
#'
#' @param .data A data frame
#'
#' @return A tibble
#' @export
summ <- function(
    .data, ...,
    .by = NULL,
    .detail = FALSE) {
  # Identify group variables
  group_vars <- extract_group_var(.data, .by)  
  
  # Prepare data
  .data_summ <- .data %>%
    s_select(...) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -tidyselect::any_of(group_vars)
    ) %>%
    # Suppress "adding missing grouping variables" message
    suppressMessages()
  
  # Confirm variables
  check_numeric(.data_summ)
  check_missing(.data_summ)
  .data_num <- check_summarise(.data_summ)
  check_factor(.data_num)

  # Add group variables
  .data <- keep_minimal_vars(.data, .data_num, group_vars)

  print(.data)
  # .output <- summ_data_to_value(.data, vars, group_vars, .by, .stat, .detail, no_dot_by)

  # return(.output)
}
