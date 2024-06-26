# Helper functions for tab() and fre() families
s_ungroup <- function(.data) {
  if (dplyr::is_grouped_df(.data)) {
    message("Ungrouping the data.")
    .data <- dplyr::ungroup(.data)
  }
  return(.data)
}

modify_invalid_names <- function(
    .data, function_name) {
  invalid_names <-
    c(
      "n", "percent", "cum",
      "valid", "valid_cum",
      "...any_miss", "...valid_sum", "...tmp"
    )

  if (function_name %in%
    c("tab()", "tab1()")) {
    invalid_names <- invalid_names
  } else if (function_name %in%
    c("fre()", "fre1()", "tab2()", "fre2()")) {
    invalid_names <- invalid_names %>%
      c("total")
  } else {
    stop("Invalid function name.", call. = FALSE)
  }

  detected_names <- intersect(
    names(.data),
    invalid_names
  )

  if (length(detected_names) > 1) {
    names1 <- detected_names %>%
      paste0("`", ., "`") %>%
      stringr::str_flatten(
        collapse <- ", "
      )
    names2 <- detected_names %>%
      paste0("`.", ., "`") %>%
      stringr::str_flatten(
        collapse = ", "
      )
    message <- paste0(
      "To avoid duplicates in `", function_name, "`, ",
      names1, " are renamed as ", names2, ", respectively.\n",
      "Better to rename them before `", function_name, "`."
    )
    message(message)
  } else if (length(detected_names) == 1) {
    names1 <- detected_names %>%
      paste0("`", ., "`")
    names2 <- detected_names %>%
      paste0("`.", ., "`")
    message <- paste0(
      "To avoid duplicates in `", function_name, "`, ",
      names1, " is renamed as ", names2, ".\n",
      "Better to rename them before `", function_name, "`."
    )
    message(message)
  } else {
    return(.data)
  }

  can_rename <- .data %>%
    dplyr::rename_with(
      ~ paste0(".", .x),
      tidyselect::all_of(detected_names)
    ) %>%
    s_try()

  if (length(detected_names) > 0) {
    if (can_rename) {
      .data %>%
        dplyr::rename_with(
          ~ paste0(".", .x),
          tidyselect::all_of(detected_names)
        ) %>%
        return()
    } else {
      stop("The new name is also taken. Rename fails.", call. = FALSE)
    }
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
    stop(message1, call. = FALSE)
  } else if (var_n > 2) {
    stop(message2, call. = FALSE)
  }
}

prepare_tab_data <- function(.data, ..., group_vars = character(0)) {
  if (group_vars %>% length() > 0) {
    out <- .data %>%
      remove_group_vars(..., group_vars = group_vars)
  } else {
    if (!missing(...)) {
      .data <- s_select(.data, ...)
    }
    out <- .data %>%
      dplyr::ungroup()
  }
  return(out)
}

tab_data <- function(
    .data, group_vars = character(0), .desc = FALSE) {
  out <- .data %>%
    dplyr::count(
      dplyr::across(
        dplyr::everything()
      )
    )

  if (group_vars %>% length() > 0) {
    out <- out %>%
      dplyr::group_by(
        dplyr::pick(
          dplyr::all_of(group_vars)
        )
      )
  }

  out <- out %>%
    dplyr::mutate(
      percent = n / sum(n) * 100,
      ...any_miss = dplyr::if_any(
        dplyr::everything(), ~ is.na(.)
      ),
      ...valid_sum = dplyr::if_else(
        ...any_miss, 0, percent
      ) %>% sum(),
      valid = dplyr::if_else(
        ...any_miss, NA_real_, percent / ...valid_sum * 100
      )
    )

  if (.desc) {
    out <- dplyr::arrange(out, -valid)
  } else {
    out <- dplyr::arrange(out, ...any_miss)
  }

  out <- out %>%
    dplyr::mutate(
      cum = cumsum(percent),
      valid_cum = cumsum(valid)
    )

  if (group_vars %>% length() > 0) {
    out <- out %>%
      dplyr::ungroup()
  }

  out <- out %>%
    dplyr::relocate(
      percent, cum, valid, valid_cum,
      .after = n
    ) %>%
    dplyr::select(-...valid_sum)

  return(out)
}

add_total <- function(data) {
  narrow_data <- ncol(data) <= 11

  if (narrow_data) {
    col_name <- data %>%
      dplyr::select(
        -c(n:dplyr::last_col())
      ) %>%
      ds(dplyr::last_col())

    data <- data %>%
      dplyr::rename(...tmp = {{ col_name }}) %>%
      dplyr::mutate(...tmp = ...tmp %>% as_character())
  } else {
    data <- data %>%
      dplyr::mutate(...tmp = "") %>%
      dplyr::relocate(
        ...tmp,
        tidyselect::last_col(6):tidyselect::last_col(2)
      )
  }

  out_head <- data %>%
    dplyr::filter(!...any_miss)
  out_tail <- data %>%
    dplyr::filter(...any_miss)

  if (nrow(out_head) > 1 && nrow(out_tail) > 0) {
    out_head <- out_head %>%
      dplyr::add_row(
        ...tmp = "Valid Total",
        n = dplyr::pull(., n) %>%
          sum(),
        percent = dplyr::pull(., percent) %>%
          sum(),
        valid = 100
      )
  }
  if (nrow(out_head) > 0 && nrow(out_tail) > 1) {
    out_tail <- out_tail %>%
      dplyr::add_row(
        ...tmp = "Missing Total",
        n = dplyr::pull(., n) %>%
          sum(),
        percent = dplyr::pull(., percent) %>%
          sum()
      )
  }

  out <- out_head %>%
    dplyr::bind_rows(out_tail)
  if (nrow(out) > 0) {
    out <- out %>%
      dplyr::add_row(
        ...tmp = "Total",
        n = dplyr::pull(data, n) %>%
          sum(),
        percent = 100
      )
    if (narrow_data) {
      out <- out %>%
        dplyr::rename(
          {{ col_name }} := ...tmp
        )
    } else {
      out <- out %>%
        dplyr::rename(
          total := ...tmp
        )
    }
  }
  return(out)
}

append_tab_list <- function(.data, blank_row = FALSE) {
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
        s_type(.abbr = TRUE) %>%
        dplyr::if_else(
          . %in% c("int", "dbl"), "num", .
        )
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

  if (blank_row) {
    out <- out %>%
      purrr::map(
        ~ .x %>%
          dplyr::add_row(
            variable = NA_character_
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
      ...tmp := 1
    ) %>%
    dplyr::mutate(
      dplyr::across(
        -1, ~ tidyr::replace_na(.x, 0)
      ),
      ...tmp := ...tmp %>%
        as_character()
    )

  out_sum <- out %>%
    dplyr::mutate(
      dplyr::across(-1, ~ sum(out$.)),
      ...tmp := "total"
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
      {{ join_name }} := ...tmp
    )

  return(out)
}

result_to_flextable <- function(tb, group_vars, fre1 = FALSE, fre2 = FALSE) {
  if (fre2) {
    tb <- tb %>%
      dplyr::mutate(
        dplyr::across(
          1,
          ~ tidyr::replace_na(.x, "NA")
        )
      )
  } else if (fre1) {
    tb <- tb %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(
            c(group_vars, "value")
          ),
          ~ ifelse(
            is.na(variable), NA,
            tidyr::replace_na(.x, "NA")
          )
        )
      )
  } else {
    tb <- tb %>%
      dplyr::mutate(
        dplyr::across(
          -dplyr::any_of(
            c("n", "percent", "cum", "valid", "valid_cum")
          ),
          ~ tidyr::replace_na(.x, "NA")
        )
      )
  }
  ft <- tb %>%
    flextable::as_flextable(
      max_row = 1e3
    )

  return(ft)
}
