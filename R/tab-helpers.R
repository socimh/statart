# Helper functions for tab() and fre() families
select_or_transmute <- function(.data, ...) {
  select_works <- s_try(
    dplyr::select(.data, ...)
  )
  mutate_works <- s_try(
    dplyr::transmute(.data, ...)
  )
  empty_dots <- missing(...)

  if (!select_works && !mutate_works && !empty_dots) {
    stop("Neither <tidy-select> (i.e., `select()`) nor <data-masking> (e.g., `mutate()`) works.
    Please check your input.")
  }

  if (!empty_dots) {
    if (select_works) {
      .data <- dplyr::select(
        .data, ...
      )
    } else {
      .data <- dplyr::transmute(
        .data, ...
      )
    }
  }
  return(.data)
}

s_ungroup <- function(.data) {
  if (dplyr::is_grouped_df(.data)) {
    message("Ungrouping the data.")
    .data <- dplyr::ungroup(.data)
  }
  return(.data)
}

check_invalid_names <- function(
    .data, function_name) {
  invalid_names <-
    c(
      "n", "percent", "cum",
      "valid", "valid_cum", 
      ".any_miss", ".valid_sum", ".tmp"
    )
  
  if (function_name %in% 
   c("tab()", "tab1()", "fre()", "fre1()")) {
    invalid_names <- invalid_names
  } else if (function_name %in% 
   c("tab2()", "fre2()")) {
    invalid_names <- invalid_names %>% 
      c("total")
  } else {
    stop("Invalid function name.")
  }

  detected_names <- intersect(
    names(.data),
    invalid_names
  )
  detected_invalid <- length(detected_names) > 0
  if (detected_invalid) {
    stringr::str_flatten(
      detected_names,
      collapse = ", "
    ) %>%
      paste0(
        " are invalid names in `",
        function_name,
        "`. Please rename them before using the function."
      ) %>%
      stop()
  }
}

check_two_vars <- function(.data) {
  var_n <- ncol(.data)
  message1 <- paste0(
    "Only ",
    var_n,
    " variable is input.
      Please provide two variables for cross-tabulation."
  )
  message2 <- paste0(
    var_n,
    " variables are input.
      Please provide only two variables for cross-tabulation."
  )
  if (var_n < 2) {
    stop(message1)
  } else if (var_n > 2) {
    stop(message2)
  }
}

tab_data <- function(
    .data, .desc = FALSE) {
  out <- .data %>%
    dplyr::count(
      dplyr::across(
        dplyr::everything()
      )
    ) %>%
    dplyr::mutate(
      percent = n / sum(n) * 100,
      .any_miss = dplyr::if_any(
        dplyr::everything(), ~ is.na(.)
      ),
      .valid_sum = dplyr::if_else(
        .any_miss, 0, percent
      ) %>% sum(),
      valid = dplyr::if_else(
        .any_miss, NA_real_, percent / .valid_sum * 100
      )
    )

  if (.desc) {
    out <- dplyr::arrange(out, -valid)
  } else {
    out <- dplyr::arrange(out, .any_miss)
  }

  out <- out %>%
    dplyr::mutate(
      cum = cumsum(percent),
      valid_cum = cumsum(valid)
    ) %>%
    dplyr::relocate(
      percent, cum, valid, valid_cum,
      .after = n
    ) %>%
    dplyr::select(-.valid_sum)

  return(out)
}

add_total <- function(out) {
  col_name <- out %>%
    dplyr::select(
      -c(n:dplyr::last_col())
    ) %>%
    ds(dplyr::last_col()) %>%
    suppressMessages()

  out <- out %>%
    dplyr::rename(
      .tmp = {{ col_name }}
    ) %>%
    dplyr::mutate(
      .tmp = .tmp %>%
        as_character()
    )

  out_head <- out %>%
    dplyr::filter(!.any_miss) %>%
    dplyr::add_row(
      .tmp = "Valid Total",
      n = dplyr::pull(., n) %>%
        sum(),
      percent = dplyr::pull(., percent) %>%
        sum(),
      valid = 100
    )
  out_tail <- out %>%
    dplyr::filter(.any_miss) %>%
    dplyr::add_row(
      .tmp = "Missing Total",
      n = dplyr::pull(., n) %>%
        sum(),
      percent = dplyr::pull(., percent) %>%
        sum()
    )
  if (nrow(out_tail) == 2) {
    out_tail <- out_tail %>%
      dplyr::slice(1)
  }

  out <- out_head %>%
    dplyr::bind_rows(out_tail) %>%
    dplyr::add_row(
      .tmp = "Total",
      n = dplyr::pull(., n) %>%
        sum(),
      percent = 100
    ) %>%
    dplyr::rename(
      {{ col_name }} := .tmp
    )
  return(out)
}

append_tab_list <- function(.data) {
  out <- .data %>%
    purrr::imap(
      ~ dplyr::mutate(
        .x,
        variable = .y
      )
    )

  vec_types <- out %>%
    purrr::map_chr(
      ~ dplyr::pull(., value) %>%
        s_vec_type()
    ) %>%
    unique() %>%
    length()

  if (vec_types > 1) {
    out <- out %>%
      purrr::map(
        ~ .x %>%
          dplyr::mutate(
            value = as_character(value)
          )
      )
  }

  out <- out %>%
    purrr::list_rbind() %>%
    dplyr::relocate(variable)

  return(out)
}

tab2_data <- function(.data) {
  join_name <- .data %>%
    names() %>%
    stringr::str_flatten(" \\ ")

  out <- .data %>%
    dplyr::count(
      dplyr::across(
        dplyr::everything()
      )
    ) %>%
    tidyr::pivot_wider(
      names_from = 2,
      values_from = n
    ) %>%
    dplyr::mutate(
      dplyr::across(-1, ~ tidyr::replace_na(.x, 0))
    ) %>%
    dplyr::rename(
      {{ join_name }} := 1
    )

  return(out)
}

add_total2 <- function(out, .data) {
  join_name <- .data %>%
    names() %>%
    stringr::str_flatten(" \\ ")

  out <- out %>%
    dplyr::rename(
      .tmp := 1
    ) %>%
    dplyr::mutate(
      dplyr::across(
        -1, ~ tidyr::replace_na(.x, 0)
      ),
      .tmp := .tmp %>%
        as_character()
    )
  
  out_sum <- out %>%
    dplyr::mutate(
      dplyr::across(-1, ~ sum(out$.)),
      .tmp := "total"
    ) %>%
    dplyr::slice(1)
  
  out <- out %>%
    dplyr::bind_rows(out_sum) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total = dplyr::c_across(-1) %>%
        sum()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(
      {{ join_name }} := .tmp
    )
  
  return(out)
}