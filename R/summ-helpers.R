extract_group_var <- function(.data, by) {
  any_dot_by <- !rlang::quo_is_null(by)
  any_group_by <- dplyr::is_grouped_df(.data)

  if (!any_dot_by && any_group_by) {
    group_vars <- dplyr::group_vars(.data)
  } else if (any_dot_by) {
    group_vars <- ds(.data, !!by)
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

remove_group_vars <- function(.data, ..., group_vars) {
  if (!missing(...)) {
    .data <- s_select(.data, ...)
  }
  out <- .data %>%
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

is_summable_chr <- function(chr) {
  chr %>%
    stringr::str_detect(
      "^(lgl|int|dbl|int64|units|drtn|time|date|dttm|fct|ord)"
    )
}

is_summable <- function(.x) {
  s_type(.x, .abbr = TRUE) %>%
    is_summable_chr()
}

check_summarise <- function(.data) {
  out <- .data %>%
    dplyr::select(tidyselect::where(
      ~ is_summable(.x) &
        mean(is.na(.x)) < 1L
    ))
  summ_vars <- names(out)

  if (length(summ_vars) == 0) {
    stop(
      "None of the provided variable(s) is suitable for `summ()`.",
      call. = FALSE
    )
  } else {
    return(out)
  }
}

warning_vars <- function(vars, suffix) {
  if (length(vars) == 0) {
    return()
  } else if (length(vars) == 1) {
    message <- paste0(vars, " is ", suffix)
    warning(message, call. = FALSE)
  } else if (
    stringr::str_length(vars[1]) +
      stringr::str_length(vars[2]) > 40) {
    message <- paste0(
      vars[1], ", ... are ", suffix
    )
    warning(message, call. = FALSE)
  } else if (length(vars) > 2) {
    message <- paste(vars[1:2], collapse = ", ") %>%
      paste0(", ... are ", suffix)
    warning(message, call. = FALSE)
  } else {
    message <- paste(vars, collapse = " and ") %>%
      paste0(" are ", suffix)
    warning(message, call. = FALSE)
  }
}

check_numeric <- function(.data, .keep_all) {
  # Check if vars are numeric
  non_num_vars <- .data %>%
    ds(!tidyselect::where(
      ~ is_summable(.x)
    ))

  suffix <- dplyr::if_else(
    .keep_all,
    "non-numeric (***).",
    "non-numeric and thus removed."
  )
  warning_vars(non_num_vars, suffix)
}

check_missing <- function(.data, .keep_all) {
  na_vars <- .data %>%
    ds(tidyselect::where(
      ~ is_summable(.x) &
        mean(is.na(.)) == 1L
    ))

  suffix <- dplyr::if_else(
    .keep_all,
    "entirely missing.",
    "entirely missing and thus removed."
  )
  warning_vars(na_vars, suffix)
}

check_factor <- function(.data) {
  fct_vars <- .data %>%
    ds(tidyselect::where(
      ~ s_type(.x, .abbr = TRUE) %in% c("fct", "ord")
    ))

  suffix <- dplyr::if_else(
    length(fct_vars) == 1,
    "a factor (**).",
    "factors (**)."
  )
  warning_vars(fct_vars, suffix)
}

check_label <- function(.data) {
  lbl_vars <- .data %>%
    ds(tidyselect::where(
      ~ s_type(.x, .abbr = TRUE) %>%
        stringr::str_detect("lbl$")
    ))

  suffix <- dplyr::if_else(
    length(lbl_vars) == 1,
    "a labelled variable (*).",
    "labelled variables (*)."
  )
  warning_vars(lbl_vars, suffix)
}

summ_var <- function(var, stat = character(0), .detail = FALSE) {
  if (length(stat) == 0 && !.detail) {
    stat_tb <- tibble::tibble(
      type = dplyr::if_else(
        !is.na(s_unit(var)),
        stringr::str_glue(
          "{s_type(var, .abbr = TRUE)} [{s_unit(var)}]"
        ),
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
        stringr::str_glue(
          "{s_type(var, .abbr = TRUE)} [{s_unit(var)}]"
        ),
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
  )

  if (!is.null(numeric) && nrow(numeric) > 0) {
    numeric <- numeric %>%
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
        name = dplyr::case_when(
          !is_summable_chr(type) ~
            stringr::str_glue("***{name}"),
          type %in% c("fct", "ord") ~
            stringr::str_glue("**{name}"),
          stringr::str_detect(type, "lbl$") ~
            stringr::str_glue("*{name}"),
          TRUE ~ name
        ) %>% as.character()
      )
  }

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
    message(
      "A list is returned as statistics are heterogeneous.",
      call. = FALSE
    )
  }

  return(out)
}
