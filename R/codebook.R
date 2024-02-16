codebook <- function(.data, ...) {
  # if ... is empty, select all variables
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)
  if (length(pos) == 0) {
    pos <- seq_along(.data)
  }
  .data <- .data[pos]

  .output <- .data %>%
    summarise(
      across(everything(), read_var_type)
    ) %>%
    pivot_longer(
      everything(),
      names_to = "variable"
    ) %>%
    unnest_wider(value) %>%
    mutate(
      type = if_else(
        !is.na(unit),
        paste0(type, " [", unit, "]"),
        type
      )
    ) %>%
    select(-unit)

  contents <- Hmisc::contents(.data)$contents %>%
    tibble()

  if ("Labels" %in% names(contents)) {
    .output <- .output %>%
      bind_cols(
        contents %>%
          select(label = Labels)
      )
  }

  return(.output)
}