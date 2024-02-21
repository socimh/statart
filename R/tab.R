#' Tabulate columns
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Enhanced `count()`.
#'
#' * `tab()` - Tabulates variable(s) simultaneously, similar to `count()`.
#' * `tab1()` - Tabulates variable(s) one by one.
#' * `tab2()` - Cross-tabulates two variables.
#' * `fre()` - Adds “Total” row(s) to the output of `tab()`.
#' * `fre1()` - Adds “Total” row(s) to the output of `tab1()`.
#' * `fre2()` - Adds a “Total” row and a “total” column to the output of `tab2()`.
#'
#' Overall, `tab()` functions are suitable for
#' subsequent data analysis, while `fre()` functions
#' are suitable for printing.
#'
#' If the output tibble has many rows, consider using
#' `print()` or [`s_print()`][s_print] to display the output.
#'
#' @param .data A data frame, data frame extension
#' (e.g. a tibble), or a lazy data frame (e.g. from dbplyr or dtplyr).
#' @param ... <[`tidy-select`][dplyr_tidy_select]> or
#' <[`data-masking`][args_data_masking]> Variables to tabulate.
#'
#' Both tidyselect (e.g., `starts_with()`) and
#' data masking (e.g., `x_sq = x^2`) are supported.
#' See examples below.
#' @param .desc If TRUE, will show the largest groups at the top.
#' @param .append If TRUE, will return a single tibble
#' instead of a list of tibbles.
#' @param .flip If TRUE, will transpose the output tibble.
#'
#' @return
#' An object of the same type as `.data`.
#'
#' @name tab
#'
#' @examples
#' tab(starwars, 8)
#' tab(starwars, sex)
#' tab(starwars, sex, .desc = TRUE)
#' fre(starwars, sex)
#' fre(starwars, sex, .desc = TRUE)
#' 
#' tab(starwars, sex, gender)
#' tab(starwars, sex == "male")
#' tab(starwars, male = sex == "male")
#' tab(starwars, ends_with("color"))
#' tab(starwars, s_matches("s*e"))
#' tab(starwars)
#'
#' tab1(starwars, 1:3)
#' tab1(starwars, ends_with("color"))
#' tab1(starwars, ends_with("color"), .append = TRUE)
#' fre1(starwars, ends_with("color"), .append = TRUE)
#' fre1(starwars, ends_with("color"), .append = TRUE) %>% s_print()
#'
#' tab2(starwars, sex, gender)
#' tab2(starwars, height, gender)
#' fre2(starwars, height, gender)
#' fre2(starwars, height, gender) %>% s_print()
NULL

#' @export
#' @rdname tab
tab <- function(.data, ..., .desc = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  .data <- modify_invalid_names(.data, "tab()")

  # Tabulate
  out <- tab_data(.data, .desc) %>%
    dplyr::select(-...any_miss)

  # Output
  return(out)
}

#' @export
#' @rdname tab
tab1 <- function(
    .data, ...,
    .desc = FALSE,
    .append = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  .data <- modify_invalid_names(.data, "tab1()")

  # Tabulate as a list
  out <- .data %>%
    purrr::map(
      ~ tibble::tibble(value = .) %>%
        tab_data(.desc) %>%
        dplyr::select(-...any_miss)
    )

  # Bind rows if .append
  if (.append) {
    out <- append_tab_list(out)
  }

  # Output
  return(out)
}

#' @export
#' @rdname tab
tab2 <- function(
    .data, ..., .flip = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  check_two_vars(.data)
  .data <- modify_invalid_names(.data, "tab2()")

  # Flip variables
  if (.flip) {
    .data <- .data %>%
      dplyr::relocate(2, 1)
  }

  # Cross-tabulate
  out <- tab2_data(.data)

  # Output
  return(out)
}

#' @export
#' @rdname tab
fre <- function(
    .data, ..., .desc = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  .data <- modify_invalid_names(.data, "fre()")

  # Tabulate
  out <- .data %>%
    tab_data(.desc) %>%
    add_total() %>%
    dplyr::select(-...any_miss)

  # Output
  return(out)
}

#' @export
#' @rdname tab
fre1 <- function(
    .data, ..., .desc = FALSE, .append = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  .data <- modify_invalid_names(.data, "fre1()")

  # Tabulate as a list
  out <- .data %>%
    purrr::map(
      ~ tibble::tibble(value = .) %>%
        tab_data(.desc) %>%
        add_total() %>%
        dplyr::select(-...any_miss)
    )

  # Bind rows if .append
  if (.append) {
    out <- append_tab_list(out)
  }

  # Output
  return(out)
}


#' @export
#' @rdname tab
fre2 <- function(
    .data, ..., .flip = FALSE) {
  # Prepare data
  .data <- .data %>%
    s_select(...)
  .data <- s_ungroup(.data)

  # Confirm names
  check_two_vars(.data)
  .data <- modify_invalid_names(.data, "fre2()")

  # Flip variables
  if (.flip) {
    .data <- .data %>%
      dplyr::relocate(2, 1)
  }

  # Cross-tabulate
  out <- .data %>%
    tab2_data() %>%
    add_total2(.data)

  # Output
  return(out)
}
