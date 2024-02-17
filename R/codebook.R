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
