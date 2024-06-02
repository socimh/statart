#' Summarise columns
#'
#' `r lifecycle::badge("experimental")`
#' Enhanced `summarise()`.
#'
#' @inheritParams tab
#' @inheritParams dplyr::summarise
#' @param .keep_all A logical. If `TRUE`, all variables are kept.
#' @param .detail A logical. If `TRUE`, the detailed summary is returned.
#' @param .stat A character vector.
#' If specified, only the listed statistics are returned.
#' @name summ
#'
#' @return A tibble
#' @export
#'
#' @examples
#' summ(lifeexp)
#'
#' summ(lifeexp, .by = region)
#'
#' tabstat(lifeexp, .by = region)
#'
#' lifeexp %>%
#'   summ(
#'     dplyr::where(is.numeric),
#'     .by = region,
#'     .stat = c("mean", "sd")
#'   )
summ <- function(
    .data, ...,
    .by = NULL,
    .keep_all = FALSE,
    .detail = FALSE,
    .stat = character(0)) {
  # Identify group variables
  by <- rlang::enquo(.by)
  group_vars <- extract_group_var(.data, by)

  # Prepare data
  .data_summ <- remove_group_vars(
    .data, ...,
    group_vars = group_vars
  )

  # Confirm variables
  check_numeric(.data_summ, .keep_all)
  check_missing(.data_summ, .keep_all)
  .data_num <- check_summarise(.data_summ)
  check_factor(.data_num)
  check_label(.data_num)

  # Add group variables
  if (.keep_all == TRUE) {
    .data <- keep_useful_vars(.data, .data_summ, group_vars)
  } else {
    .data <- keep_useful_vars(.data, .data_num, group_vars)
  }

  out <- summ_list(.data, group_vars, .detail, .stat)
  return(out)
  # .output <- summ_data_to_value(.data, vars, group_vars, .by, .stat, .detail, no_dot_by)

  # return(.output)
}

#' @rdname summ
#' @export
tabstat <- function(
    .data, ...,
    .by = NULL,
    .keep_all = FALSE,
    .detail = FALSE,
    .stat = "mean") {
  return(
    summ(
      .data, ...,
      .by = {{ .by }},
      .keep_all = .keep_all,
      .detail = .detail,
      .stat = .stat
    )
  )
}
