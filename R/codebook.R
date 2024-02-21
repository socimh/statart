#' View the codebook of a dataset or selected variables.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `codebook()` is a function to view the
#' codebook of a dataset or selected variables.
#'
#' @param .data The input data (data frame or tibble).
#' @param ...  <[`tidy-select`][dplyr_tidy_select]> One or more
#' unquoted expressions separated by commas. Variable names can be
#' used as if they were positions in the data frame, so expressions
#' like x:y can be used to select a range of variables.
#' *This argument can be omitted*.
#' @return  An object of the same type as `.data`.
#' @export
#'
#' @examples
#' starwars
#' codebook(starwars)
#' codebook(starwars, 1:4)
#' codebook(starwars, ends_with("color"))
#' codebook(starwars, where(is.numeric))
#' lifeexp
#' codebook(lifeexp)
codebook <- function(.data, ...) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    .data <- dplyr::select(.data, ...)
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

  # Hmisc::contents() cannot include "units" variables.
  .data_no_unit <- .data %>%
    dplyr::select(
      tidyselect::where(
        ~ s_type(.x) != c("units")
      )
    )

  contents <- Hmisc::contents(.data_no_unit)$contents %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      variable = names(.data_no_unit)
    ) %>%
    dplyr::select(
      tidyselect::any_of(c("variable", "Labels"))
    )

  if (ncol(contents) == 2) {
    out <- out %>%
      dplyr::left_join(
        contents %>%
          dplyr::rename(label = Labels), 
          by = join_by(variable)
      )
  }

  return(out)
}

# Helper function:
codebook_var <- function(var) {
  stat_tb <- tibble::tibble(
    type = s_type(var, .full = TRUE),
    unit = s_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = dplyr::n_distinct(var, na.rm = TRUE)
  )
  return(stat_tb)
}
