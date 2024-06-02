#' Show the variable list of a dataset
#'
#' @description
#' * `variables()` works like the "Variables" tab in Stata.
#' * `variables_search()` searches for variables by name or label.
#'
#' @param .data The input data (data frame or tibble).
#' @param ...  <[`tidy-select`][dplyr_tidy_select]> or
#' <[`data-masking`][args_data_masking]> Variables to include in the codebook.
#' *This argument can be omitted*.
#' @param string A string to search for in the variable names or labels.
#'
#' @return  A tibble.
#' @name variables
#' @seealso [ds()], [codebook()], [browse()]
#'
#' @examples
#' variables(lifeexp)
#'
#' variables_search(lifeexp, "n")

#' @rdname variables
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

#' @rdname variables
#' @export
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
