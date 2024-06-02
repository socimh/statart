#' View the codebook of a dataset or selected variables.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `codebook()` is a function to view the
#' codebook of a dataset or selected variables.
#'
#' @param .data The input data (data frame or tibble).
#' @param ...  <[`tidy-select`][dplyr_tidy_select]> or
#' <[`data-masking`][args_data_masking]> Variables to include in the codebook.
#' *This argument can be omitted*.
#' @param .type The output type. Default for `codebook_detail()` is `"flextable"`.
#' @param n The number of rows to display. Default is `Inf`.
#' @inheritParams datawizard::data_codebook
#'
#' @return  A tibble or a flextable.
#' @name codebook
#'
#' @examples
#' starwars
#' codebook(starwars)
#'
#' codebook(starwars, 1:4)
#'
#' codebook(starwars, ends_with("color"))
#'
#' codebook(starwars, where(is.numeric))
#'
#' lifeexp
#'
#' codebook(lifeexp)

#' @rdname codebook
#' @export
codebook <- function(.data, ...) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    .data <- s_select(.data, ...)
  }

  out <- dplyr::summarise(
    .data,
    dplyr::across(tidyselect::everything(), codebook_var)
  ) %>%
    tidyr::pivot_longer(
      tidyselect::everything(),
      names_to = "variable"
    ) %>%
    tidyr::unnest_wider(value) %>%
    dplyr::mutate(
      type = dplyr::if_else(
        !is.na(unit),
        paste0(type, " [", unit, "]"),
        type
      )
    ) %>%
    dplyr::select(-unit)

  contents <- Hmisc::contents(.data)$contents %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::any_of(c("Labels")))

  if (ncol(contents) == 1) {
    out <- out %>%
      dplyr::bind_cols(
        contents %>%
          dplyr::rename(label = Labels)
      ) %>%
      dplyr::relocate(label, .after = variable)
  }

  return(out)
}

#' @rdname codebook
#' @export
codebook_detail <- function(.data, ..., .type = c("flextable", "tibble"), n = Inf, max_values = 10, range_at = 6, verbose = TRUE) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    .data <- s_select(.data, ...)
  }

  out <- .data %>%
    datawizard::data_codebook(
      max_values = max_values,
      range_at = range_at,
      verbose = verbose
    ) %>%
    tibble::as_tibble() %>%
    dplyr::rename_with(
      stringr::str_to_lower
    ) %>%
    dplyr::rename_with(
      ~ stringr::str_replace_all(., " ", "_")
    ) %>%
    dplyr::rename(row_id = .row_id)

  if (.type[1] == "flextable") {
    print(out, n = n)
  } else {
    flextable::as_flextable(out, max_row = n) %>%
      return()
  }
}

#' @rdname codebook
#' @export
variables <- function(.data, ...) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    .data <- dplyr::select(.data, ...)
  }

  out <- .data %>%
    names_as_column()

  label <- Hmisc::contents(.data)$contents %>%
    tibble::as_tibble() %>%
    dplyr::select(dplyr::any_of(c("Labels")))

  if (ncol(label) == 1) {
    out <- out %>%
      dplyr::bind_cols(
        label %>%
          dplyr::rename(label = Labels)
      )
  }

  return(out)
}

variables_search <- function(.data, string, ...) {
  if (is.character(string)) {
    string <- string %>%
      stringr::str_replace_all("\\*", ".*") %>%
      stringr::str_replace_all("~", ".+") %>%
      stringr::str_replace_all("\\?", ".") %>%
      purrr::map_chr(num_range_to_regex)
    string <- stringr::str_glue("(?i).*{string}.*")
  }

  if ("label" %in% colnames(.data)) {
    variables(.data, ...) %>%
      dplyr::filter(
        stringr::str_detect(name, string) |
          stringr::str_detect(label, string)
      ) %>%
      return()
  } else {
    variables(.data, ...) %>%
      dplyr::filter(
        stringr::str_detect(name, string)
      ) %>%
      return()
  }
}


# Helper function:
codebook_var <- function(var) {
  stat_tb <- tibble::tibble(
    type = s_type(var),
    unit = s_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = dplyr::n_distinct(var, na.rm = TRUE)
  )
  return(stat_tb)
}
