extract_group_var <- function(.data, .by) {
  any_dot_by <- length(.by) > 0
  any_group_by <- dplyr::is_grouped_df(.data)

  if (!any_dot_by && any_group_by) {
    group_vars <- dplyr::group_vars(.data)
  } else if (any_dot_by) {
    group_vars <- ds(.data, .by)
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

keep_summ_vars <- function(.data, ..., group_vars) {
  out <- .data %>%
    s_select(...) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      -tidyselect::any_of(group_vars)
    ) %>%
    # Suppress "adding missing grouping variables" message
    suppressMessages()
  return(out)
}

keep_useful_vars <- function(.data, .data_num, group_vars) {
  out <- .data %>%
    dplyr::ungroup() %>%
    dplyr::select(
      tidyselect::all_of(group_vars)
    ) %>%
    dplyr::bind_cols(.data_num)

  return(out)
}

s_type_summable <- function(var) {
  s_type(var, .abbr = TRUE) %in% c(
    "lgl", "int", "dbl", "int64", "units",
    "drtn", "time", "date", "dttm",
    "lbl", "fct", "ord"
  )
}

check_summarise <- function(.data) {
  out <- .data %>%
    dplyr::select(tidyselect::where(
      ~ s_type_summable(.x) &
        mean(is.na(.x)) < 1L
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
      ~ s_type_summable(.x)
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
      ~ s_type_summable(.x) &
        mean(is.na(.)) == 1L
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
    ds(tidyselect::where(
      ~ s_type(.x, .abbr = TRUE) %in% c("fct", "ord")
    ))

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

summ_var <- function(var, stat = character(0), .detail = FALSE) {
  if (length(stat) == 0 && !.detail) {
    stat_tb <- tibble::tibble(
      type = dplyr::if_else(
        !is.na(s_unit(var)),
        stringr::str_glue("[{s_unit(var)}]"),
        s_type(var, .abbr = TRUE)
      ) %>% as.character(),
      n = sum(!is.na(var), na.rm = TRUE),
      unique = dplyr::n_distinct(var, na.rm = TRUE),
      min = min(as_numeric(var), na.rm = TRUE),
      mean = mean(as_numeric(var), na.rm = TRUE),
      sd = sd(as_numeric(var), na.rm = TRUE),
      max = max(as_numeric(var), na.rm = TRUE)
    )
    return(stat_tb)
  } else {
      stat_tb <- tibble::tibble(
        type = dplyr::if_else(
          !is.na(s_unit(var)),
          stringr::str_glue("[{s_unit(var)}]"),
          s_type(var, .abbr = TRUE)
        ) %>% as.character(),
        n = sum(!is.na(var), na.rm = TRUE),
        unique = dplyr::n_distinct(var, na.rm = TRUE),
        miss_n = dplyr::n() - n,
        valid_pct = 1 - miss_n / (n + miss_n),
        min = min(as_numeric(var), na.rm = TRUE),
        q1 = quantile(as_numeric(var), .25, na.rm = TRUE),
        median = median(as_numeric(var), na.rm = TRUE),
        mean = mean(as_numeric(var), na.rm = TRUE),
        mad = mad(as_numeric(var), na.rm = TRUE),
        sd = sd(as_numeric(var), na.rm = TRUE),
        q3 = quantile(as_numeric(var), .75, na.rm = TRUE),
        max = max(as_numeric(var), na.rm = TRUE),
        iqr = IQR(as_numeric(var), na.rm = TRUE),
        skew = skew(as_numeric(var), sum(!is.na(var), na.rm = TRUE)),
        kurtosis = kurtosis(as_numeric(var), sum(!is.na(var), na.rm = TRUE)),
        se = sd / sqrt(n),
        # mean_sd0 = paste0(sprintf("%.0f", mean), " (", sprintf("%.0f", sd, ")")),
        # mean_sd1 = paste0(sprintf("%.1f", mean), " (", sprintf("%.1f", sd, ")")),
        # mean_sd2 = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", sd, ")")),
        # mean_sd3 = paste0(sprintf("%.3f", mean), " (", sprintf("%.3f", sd, ")"))
      )
      if (length(stat) > 0) {
        stat_tb <- stat_tb %>%
          dplyr::select(type, tidyselect::all_of(stat))
      }
      return(stat_tb)
  }  
}

summ_date <- function(var, stat = character(0), .detail = FALSE) {
  stat_tb <- tibble::tibble(
    type = dplyr::if_else(
      !is.na(s_unit(var)),
      stringr::str_glue("[{s_unit(var)}]"),
      s_type(var, .abbr = TRUE)
    ) %>% as.character(),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = dplyr::n_distinct(var, na.rm = TRUE),
    miss_n = sum(is.na(as_numeric(var)), na.rm = TRUE),
    valid_pct = 1 - miss_n / n,
    min = min(as_numeric(var), na.rm = TRUE),
    # q1 = quantile(as_numeric(var), .25, na.rm = TRUE),
    median = median(as_numeric(var), na.rm = TRUE),
    mean = mean(as_numeric(var), na.rm = TRUE),
    mad = mad(as_numeric(var), na.rm = TRUE),
    sd = sd(as_numeric(var), na.rm = TRUE),
    # q3 = quantile(as_numeric(var), .75, na.rm = TRUE),
    max = max(as_numeric(var), na.rm = TRUE),
    iqr = IQR(as_numeric(var), na.rm = TRUE),
    # skew = skew(as_numeric(var), n),
    # kurtosis = kurtosis(as_numeric(var), n),
    se = sd / sqrt(n),
    # mean_sd0 = paste0(sprintf("%.0f", mean), " (", sprintf("%.0f", sd), ")"),
    # mean_sd1 = paste0(sprintf("%.1f", mean), " (", sprintf("%.1f", sd), ")"),
    # mean_sd2 = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", sd), ")"),
    # mean_sd3 = paste0(sprintf("%.3f", mean), " (", sprintf("%.3f", sd), ")")
  )
  if (length(stat) > 0) {
    stat_tb <- stat_tb %>%
      dplyr::select(type, tidyselect::all_of(stat))
  } else if (!.detail) {
    stat_tb <- stat_tb %>%
      dplyr::select(
        type, n, unique,
        mean, sd, min, max
      )
  }
  return(stat_tb)
}

summ_data <- function(.data, .fn, summ_vars, group_vars, .detail, .stat) {
  if (length(summ_vars) > 0) {
    out <- .data %>%
      dplyr::summarise(
        dplyr::across(
          tidyselect::all_of(summ_vars),
          ~ .fn(.x, .stat, .detail)
        ),
        .by = tidyselect::all_of(group_vars)
      ) %>%
      suppressWarnings() %>%
      tidyr::pivot_longer(
        !tidyselect::all_of(group_vars)
      ) %>%
      tidyr::unnest(value)
  } else {
    out <- NULL
  }
  return(out)
}

summ_list <- function(.data, group_vars, .detail, .stat) {
  numeric <- .data %>%
    ds(
      !tidyselect::all_of(group_vars) &
        !tidyselect::where(
          ~ s_type(.x) %in% c("date", "datetime")
        )
    )

  date <- .data %>%
    ds(
      !tidyselect::all_of(group_vars) &
        tidyselect::where(
          ~ s_type(.x) %in% c("date")
        )
    )

  datetime <- .data %>%
    ds(
      !tidyselect::all_of(group_vars) &
        tidyselect::where(
          ~ s_type(.x) %in% c("datetime")
        )
    )

  numeric <- summ_data(
    .data, summ_var, numeric, group_vars, .detail, .stat
  ) %>%
    dplyr::mutate(
      dplyr::across(
        type:tidyselect::last_col() &
          tidyselect::where(
            ~ is.numeric(.x)
          ),
        ~ dplyr::if_else(
          is.na(.x) | !is.finite(.x),
          NA_real_, .x
        )
      ),
      name = dplyr::if_else(
        type %in% c("fct", "ord", "lbl"),
        stringr::str_glue("{name}***"),
        name
      ) %>% as.character()
    )

  date <- summ_data(
    .data, summ_date, date, group_vars, .detail, .stat
  )

  datetime <- summ_data(
    .data, summ_date, datetime, group_vars, .detail, .stat
  )

  out <- list(
    numeric = numeric,
    date = date,
    datetime = datetime
  ) %>%
    purrr::discard(~ is.null(.))

  if (length(out) == 1) {
    out <- out[[1]]
  } else {
    warning("A list is returned. Use `purrr::map()` to manipulate the tibbles.")
  }

  return(out)
}
