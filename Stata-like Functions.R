# install.packages("haven")
# install.packages("Hmisc")
# library(Hmisc)
# library(haven)
# library(tidyverse)

read_var_type <- function(var) {
  stat_tb <- tibble(
    type = vctrs::vec_ptype_full(var) %>%
      str_extract("^\\w+"),
    unit = get_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = n_distinct(var, na.rm = TRUE)
  )
  return(stat_tb)
}


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

# TBD:
# - codebook() of factor variables.
# - codebook() of many variables.


fre <- function(data, ..., .desc = FALSE) {
  nvars <- data %>%
    select(...) %>%
    ncol()
  result <- data %>%
    count(...) %>%
    mutate(
      percent = n / sum(n) * 100,
      sum_valid = if_else(if_any(all_of(1:nvars), ~ is.na(.)), 0, percent) %>% sum(),
      valid = if_else(if_any(all_of(1:nvars), ~ is.na(.)), NA_real_, percent / sum_valid * 100)
    )
  if (.desc) {
    result <- result %>%
      arrange(-valid)
  }
  result <- result %>%
    mutate(cum = cumsum(valid)) %>%
    select(-sum_valid)
  result %>% print(n = 50)
}

tab <- function(data, var1, var2, na.rm = FALSE, .desc = FALSE, .print = TRUE) {
  if (missing(var2)) {
    result <- data %>%
      count({{ var1 }})
    if (na.rm) {
      result <- result %>%
        filter(!is.na({{ var1 }}))
    }
    result <- result %>%
      mutate(percent = n / sum(n) * 100)
    if (.desc) {
      result <- result %>%
        arrange(-percent)
    }
    result <- result %>%
      mutate({{ var1 }} := {{ var1 }} %>% as.character(),
        cum = cumsum(percent)
      ) %>%
      add_row({{ var1 }} := "total",
        n = sum(.$n),
        percent = 100
      )
  } else {
    result <- data %>%
      count({{ var1 }}, {{ var2 }}) %>%
      pivot_wider(names_from = {{ var2 }}, values_from = n) %>%
      mutate(
        across(-1, ~ replace_na(.x, 0)),
        {{ var1 }} := {{ var1 }} %>%
          as.character()
      ) # replace NAs with 0s
    result_sum <- result %>%
      mutate(
        across(-1, ~ sum(result$.)),
        {{ var1 }} := "total"
      ) %>%
      slice(1)
    result <- result %>%
      bind_rows(result_sum) %>%
      rowwise() %>%
      mutate(total = sum(c_across(-1))) %>%
      ungroup()
  }
  if (.print) {
    result %>% print(n = 50)
  } else {
    return(result)
  }
}

tab1 <- function(data, ..., text = TRUE, .append = FALSE) {
  result <- data %>%
    select(...) %>%
    map(as_tibble) %>%
    map(~ tab(., value, .print = FALSE))

  if (.append) {
    result <- result %>%
      imap_dfr(~ mutate(.x, variable = .y)) %>%
      relocate(variable)

      print(result, n = 50)
  } else {
    print(result)
  }
}

get_type_abbr <- function(.x) {
  type <- vctrs::vec_ptype_abbr(.x)
  if (vctrs::vec_ptype_full(.x) %>%
    str_detect("^labelled")) {
    type <- "lbl"
  }
  return(type)
}

get_unit <- function(x) {
  unit <- NA_character_

  if (get_type_abbr(x) %in% c("units", "drtn", "time")) {
    unit <- units(x) %>% as.character()
  }

  return(unit)
}

format_variable <- function(data, var) {
  data <- data %>%
    mutate({{ var }} := paste0("(", pillar::pillar_format({{ var }}), ")"))
  return(data)
}

if_numeric <- function(var) {
  if (get_type_abbr(var) %in%
    c("lbl", "units", "drtn", "time", "fct", "ord")) {
    var <- as.numeric(var)
  }
  return(var)
}

skew <- function(x, n) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  skew_val <- sum((x - mean_val)^3, na.rm = TRUE) / (n * sd_val^3)
  return(skew_val)
}

kurtosis <- function(x, n) {
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  kurtosis_val <- sum((x - mean_val)^4, na.rm = TRUE) / (n * sd_val^4) - 3
  return(kurtosis_val)
}

# Fix if everything is missing.
summ_var <- function(var, stat, .detail = FALSE) {
  stat_tb <- tibble(
    type = type_sum(var) %>%
      as.character(),
    unit = get_unit(var),
    n = sum(!is.na(var), na.rm = TRUE),
    unique = n_distinct(var, na.rm = TRUE),
    miss_n = sum(is.na(if_numeric(var)), na.rm = TRUE),
    valid_pct = 1 - miss_n / n,
    min = min(if_numeric(var), na.rm = TRUE),
    q1 = quantile(if_numeric(var), .25, na.rm = TRUE),
    median = median(if_numeric(var), na.rm = TRUE),
    mean = mean(if_numeric(var), na.rm = TRUE),
    mad = mad(if_numeric(var), na.rm = TRUE),
    sd = sd(if_numeric(var), na.rm = TRUE),
    q3 = quantile(if_numeric(var), .75, na.rm = TRUE),
    max = max(if_numeric(var), na.rm = TRUE),
    iqr = IQR(if_numeric(var), na.rm = TRUE),
    skew = skew(if_numeric(var), n),
    kurtosis = kurtosis(if_numeric(var), n),
    se = sd / sqrt(n),
    # mean_sd0 = paste0(sprintf("%.0f", mean), " (", sprintf("%.0f", sd), ")"),
    # mean_sd1 = paste0(sprintf("%.1f", mean), " (", sprintf("%.1f", sd), ")"),
    # mean_sd2 = paste0(sprintf("%.2f", mean), " (", sprintf("%.2f", sd), ")"),
    # mean_sd3 = paste0(sprintf("%.3f", mean), " (", sprintf("%.3f", sd), ")")
  )
  if (length(stat) > 0) {
    stat_tb <- stat_tb %>%
      select(type, unit, all_of(stat))
  } else if (!.detail) {
    stat_tb <- stat_tb %>%
      select(
        type, unit, n, unique,
        mean, sd, min, max
      )
  }
  return(stat_tb)
}

select_names <- function(.data, ...) {
  .data %>%
    ungroup() %>%
    select(...) %>%
    names()
}

match_names <- function(.data, ...) {
  .data %>%
    ungroup() %>%
    select(matches(...)) %>%
    names()
}

loc_vec_type <- function(.x) {
  vec_type <- get_type_abbr(.x)
  vec_type_regroup <- case_when(
    vec_type %in%
      c("lgl", "int", "dbl", "int64", "lbl", "units", "drtn", "time") ~ "num",
    # vec_type %in% c("fct", "ord") ~ "fct",
    # vec_type %in% c("date", "dttm") ~ "dttm",
    TRUE ~ vec_type
  )
  return(vec_type_regroup)
}

is_summable <- function(.x) {
  get_type_abbr(.x) %in%
    c(
      "lgl", "int", "dbl", "int64",
      "drtn", "time", "units",
      "lbl", "fct", "ord"
    )
}

summ_data_to_value <- function(.data, vars, group_vars, .by, .stat, .detail, no_by) {
  # if .by is not specified
  if (no_by) {
    .output <- .data %>%
      summarise(
        across(
          all_of(vars),
          ~ summ_var(.x, .stat, .detail = .detail)
        )
      )
  } else {
    # if .by is specified
    .output <- .data %>%
      summarise(
        across(
          all_of(vars),
          ~ summ_var(.x, .stat, .detail = .detail)
        ),
        .by = all_of(.by)
      )
  }

  .output %>%
    pivot_longer(
      -all_of(group_vars),
      names_to = "variable"
    ) %>%
    unnest_wider(value) %>%
    mutate(
      variable = if_else(
        type %>%
          str_detect("fct|ord|lbl"),
        paste0(variable, "***"),
        variable
      ),
      type = if_else(
        !is.na(unit),
        paste0("[", unit, "]"),
        type
      )
    ) %>%
    select(-unit)
}


summ <- function(.data, vars, .by = NULL, .detail = FALSE, .stat = character(0)) {
  # convert .by to a character vector
  .by <- .data %>%
    select_names({{ .by }})

  # Check if vars are numeric
  nonnum_vars <- .data %>%
    select_names({{ vars }} &
      where(~ !is_summable(.) &
        !(get_type_abbr(.x) %in% c("date", "dttm"))))
  na_vars <- .data %>%
    select_names({{ vars }} & where(~ mean(is.na(.)) == 1L))
  date_vars <- .data %>%
    select_names({{ vars }} &
      where(~ get_type_abbr(.x) %in% c("date", "dttm")))
  fct_vars <- .data %>%
    select_names({{ vars }} &
      where(~ get_type_abbr(.x) %in% c("fct", "ord")))
  vars <- .data %>%
    select_names({{ vars }} &
      where(~ is_summable(.) & mean(is.na(.)) < 1L))

  if (length(nonnum_vars) == 1) {
    warning(paste0("
    ", nonnum_vars, " is non-numeric.
    Use `tab()` instead.\n"))
  }
  if (length(nonnum_vars) > 1) {
    warning(paste(nonnum_vars, collapse = ", ") %>%
      paste0("
      ", ., " are non-numeric.
      Use `tab1()` instead.\n"))
  }
  if (length(date_vars) == 1) {
    warning(paste0("
    ", date_vars, " is a date or datetime variable.
    Use `summ_date()` or `summ_all()` instead.\n"))
  }
  if (length(date_vars) > 1) {
    warning(paste(date_vars, collapse = ", ") %>%
      paste0("
      ", ., " are date or datetime variables.
      Use `summ_date()` or `summ_all()` instead.\n"))
  }
  if (length(fct_vars) == 1) {
    warning(paste0("
    ", fct_vars, " is a factor variable.
    They are summarised (***), but the statistics may be misleading.
    Consider using `tab()` instead.\n"))
  }
  if (length(fct_vars) > 1) {
    warning(paste(fct_vars, collapse = ", ") %>%
      paste0("
      ", ., " are factor variables.
      They are summarised (***), but the statistics may be misleading.
      Consider using `tab1()` instead.\n"))
  }
  if (length(na_vars) == 1) {
    warning(paste0("
    ", na_vars, " is entirely missing and thus removed.\n"))
  }
  if (length(na_vars) > 1) {
    warning(paste(na_vars, collapse = ", ") %>%
      paste0("
      ", ., " is entirely missing and thus removed.\n"))
  }
  if (length(vars) == 0) {
    stop("None of the provided variables is suitable for summarise.")
  }

  no_by <- length(.by) == 0
  no_group_by <- !is_grouped_df(.data)

  # Identify group variables
  if (no_by & !no_group_by) {
    group_vars <- group_vars(.data)
  } else if (!no_by) {
    group_vars <- .by
  } else {
    group_vars <- character(0)
  }

  .output <- summ_data_to_value(.data, vars, group_vars, .by, .stat, .detail, no_by)

  return(.output)
}


check_vars <- function(.data, vars) {
  # Check if vars are numeric
  nonnum_vars <- .data %>%
    select_names({{ vars }} & where(~ !is_summable(.)))
  na_vars <- .data %>%
    select_names({{ vars }} & where(~ mean(is.na(.)) == 1L))

  if (length(nonnum_vars) == 1) {
    warning(paste0(nonnum_vars, " is non-numeric.\n"))
  }
  if (length(nonnum_vars) > 1) {
    warning(paste(nonnum_vars, collapse = ", ") %>%
      paste0(" are non-numeric.\n"))
  }
  if (length(na_vars) == 1) {
    warning(paste0(na_vars, " is all missing.\n"))
  }
  if (length(na_vars) > 1) {
    warning(paste(na_vars, collapse = ", ") %>% paste0(" are all missing.\n"))
  }

  vars <- .data %>%
    select_names({{ vars }} & where(~ is_summable(.) & mean(is.na(.)) != 1L))

  if (length(vars) == 0) {
    stop("None of your provided variables is suitable for summarise.")
  }

  vars_list <- .data %>%
    select({{ vars }}) %>%
    summarise(across(everything(), loc_vec_type)) %>%
    pivot_longer(everything()) %>%
    # mutate(value2 = value) %>%
    group_by(value2 = value) %>%
    group_map(
      ~ pivot_wider(
        .,
        names_from = value,
        values_from = name
      ) %>%
        unnest_longer(1) %>%
        as.list()
    ) %>%
    list_flatten() %>%
    suppressWarnings()

  return(vars_list)
}

summ_all <- function(.data, vars, .by = NULL, .detail = FALSE, .stat = character(0)) {
  # convert .by to a character vector
  .by <- .data %>%
    select_names({{ .by }})

  # Check if vars are numeric
  nonnum_vars <- .data %>%
    select_names({{ vars }} & where(~ !is_summable(.)))
  na_vars <- .data %>%
    select_names({{ vars }} & where(~ mean(is.na(.)) == 1L))
  date_vars <- .data %>%
    select_names({{ vars }} &
      where(~ get_type_abbr(.x) %in% c("date", "dttm")))

  if (length(nonnum_vars) == 1) {
    warning(paste0(nonnum_vars, " is non-numeric.\n"))
  }
  if (length(nonnum_vars) > 1) {
    warning(paste(nonnum_vars, collapse = ", ") %>% paste0(" are non-numeric.\n"))
  }
  if (length(na_vars) == 1) {
    warning(paste0(na_vars, " is all missing.\n"))
  }
  if (length(na_vars) > 1) {
    warning(paste(na_vars, collapse = ", ") %>% paste0(" are all missing.\n"))
  }
  if (length(date_vars) == 1) {
    warning(paste0(date_vars, " is a date or datetime variable. Use `summ_date()` or `summ_all()` instead.\n"))
  }
  if (length(date_vars) > 1) {
    warning(paste(date_vars, collapse = ", ") %>% paste0(" are date or datetime variables. Use `summ_date()` or `summ_all()` instead.\n"))
  }

  vars <- .data %>%
    select_names({{ vars }} & where(~ is_summable(.) & mean(is.na(.)) != 1L))

  if (length(vars) == 0) {
    stop("None of your provided variables is suitable for summarise.")
  }

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

  if (length(var_list) > 1) {
    warning("More than one variable type detected.\nSummarising each type separately and returning a list instead.")
  }

  no_by <- length(.by) == 0
  no_group_by <- !is_grouped_df(.data)

  # Identify group variables
  if (no_by & !no_group_by) {
    group_vars <- group_vars(.data)
  } else if (!no_by) {
    group_vars <- .by
  } else {
    group_vars <- character(0)
  }

  .output <- var_list %>%
    imap(
      ~ summ_data_to_value(.data, .x, .y, group_vars, .by, .stat, .detail, no_by)
    )

  .output <- map_dbl(.output, nrow) %>%
    order(decreasing = TRUE) %>%
    .output[.]

  if (length(.output) == 1) {
    .output <- .output[[1]]
  }

  return(.output)
}



gplot <- function(tb, ...) {
  qplot(..., data = tb) %>% return()
}
