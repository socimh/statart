extract_group_var <- function(.data, .by) {
  any_dot_by <- length(.by) > 0
  any_group_by <- dplyr::is_grouped_df(.data)

  if (!any_dot_by && any_group_by) {
    group_vars <- dplyr::group_vars(.data)
  } else if (any_dot_by) {
    group_vars <- .by
  } else {
    group_vars <- character(0)
  }

  if (!contain_vars(.data, group_vars)) {
    stop("The data do not include all grouping variable(s).")
  }

  return(group_vars)
}

contain_vars <- function(.data, group_vars) {
  contain <- .data %>%
    dplyr::ungroup() %>%
    dplyr::select(
      tidyselect::all_of(group_vars)
    ) %>%
    s_try()
  return(contain)
}

keep_minimal_vars <- function(.data, .data_num, group_vars) {
  out <- .data %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tidyselect::all_of(group_vars)
      ) %>%
    dplyr::bind_cols(.data_num)

  return(out)
}


check_summarise <- function(.data) {
  out <- .data %>%
    dplyr::select(tidyselect::where(
      ~ s_type(.x) %in% c(
        "lgl", "int", "dbl", "int64", "units",
        "drtn", "time",
        "lbl", "fct", "ord"
      ) & mean(is.na(.x)) < 1L
    ))
  summ_vars <- names(out)

  if (length(summ_vars) == 0) {
    stop("None of the provided variable(s) is suitable for `summ()`.
    Consider using `tab1()` or `fre1()` instead.")
  } else {
    return(out)
  }
}

check_numeric <- function(.data) {
  # Check if vars are numeric
  non_num_vars <- .data %>%
    ds(!tidyselect::where(
      ~ s_type(.x) %in% c(
        "lgl", "int", "dbl", "int64", "units",
        "drtn", "time",
        "lbl", "fct", "ord"
      )
    ))

  if (length(non_num_vars) == 1) {
    message <- paste0("
    ", non_num_vars, " is non-numeric.
    Consider using `tab()` or `fre()` instead.\n")
    warning(message)
  } else if (length(non_num_vars) > 1) {
    message <- non_num_vars %>%
      paste(collapse = ", ") %>%
      paste0("
      ", ., " are non-numeric.
      Consider using `tab1()` or `fre1()` instead.\n")
    warning(message)
  }
}

check_missing <- function(.data) {
  na_vars <- .data %>%
    ds(tidyselect::where(
      ~ s_type(.x) %in% c(
        "lgl", "int", "dbl", "int64", "units",
        "drtn", "time",
        "lbl", "fct", "ord"
      ) & mean(is.na(.)) == 1L
    ))

  if (length(na_vars) == 1) {
    message <- paste0("
    ", na_vars, " is entirely missing and thus removed.\n")
    warning(message)
  }
  if (length(na_vars) > 1) {
    message <- paste(na_vars, collapse = ", ") %>%
      paste0("
      ", ., " is entirely missing and thus removed.\n")
    warning(message)
  }
}

check_factor <- function(.data) {
  fct_vars <- .data %>%
    ds(tidyselect::where(~ s_type(.x) %in% c("fct", "ord")))

  if (length(fct_vars) == 1) {
    message <- paste0("
    ", fct_vars, " is a factor variable.
    They are summarised (***), but the statistics may be misleading.
    Consider using `tab()` or `fre()` instead.\n")
    warning(message)
  } else if (length(fct_vars) > 1) {
    message <- fct_vars %>%
      paste(collapse = ", ") %>%
      paste0("
      ", ., " are factor variables.
      They are summarised (***), but the statistics may be misleading.
      Consider using `tab1()` or `fre1()` instead.\n")
    warning(message)
  }
}

summ_list <- function(.data) {
  var_list <- .data %>%
    select({{ vars }}) %>%
    summarise(across(everything(), loc_vec_type)) %>%
    pivot_longer(everything()) %>%
    mutate(value2 = value) %>%
    group_by(value2, .drop = FALSE) %>%
    group_map(
      ~ pivot_wider(
        .,
        names_from = value,
        values_from = name
      ) %>%
        unnest_longer(1) %>%
        as.list()
    ) %>%
    flatten() %>%
    suppressWarnings()
}