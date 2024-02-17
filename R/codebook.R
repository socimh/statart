#' View the codebook of a dataset or selected variables.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' `codebook()` is a function to view the
#' codebook of a dataset or selected variables.
#'
#' @param .data The input data (data frame or tibble).
#' @param ...  <[`tidy-select`][dplyr_tidy_select]> One or more unquoted expressions separated by commas. Variable names can be used as if they were positions in the data frame, so expressions like x:y can be used to select a range of variables.
#' *This argument can be omitted*.
#' @returns  An object of the same type as `.data`.
#' @export
#'
#' @examples
#' codebook(starwars)
#' codebook(starwars, 1:4)
#' codebook(starwars, ends_with("color"))
#' codebook(starwars, where(is.numeric))
#' codebook(lifeexp)
codebook <- function(.data, ...) {
  # if ... is empty, select all variables
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  if (length(pos) == 0) {
    pos <- seq_along(.data)
  }
  .data <- .data[pos]

  .output <- dplyr::summarise(
    .data,
    dplyr::across(tidyselect::everything(), read_var_type)
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
    tibble::as_tibble()

  if ("Labels" %in% names(contents)) {
    .output <- dplyr::bind_cols(
      .output,
      contents %>%
        dplyr::select(label = Labels)
    )
  }

  return(.output)
}
