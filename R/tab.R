#' Tabulate columns
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' * `fre()` tabulates the frequency and missing values of one or more variables.
#' * `tab()` tabulates a single variable.
#' * `tab1()` tabulates multiple variables at once.
#' * `tab2()` cross-tabulates two variables.
#'
#' Overall, `fre()` is a more general function
#' that is an enhanced version of `count()`.
#' 
#' However, `tab()` family functions could yield
#' different results that are sometimes more useful.
#'
#' @param .data The input .data (.data frame or tibble).
#' @param row_var The variable to tabulate.
#' @param col_var (Optional) The additional variable to cross-tabulate.
#' @param sort A logical value.
#' If TRUE, sort the result in descending order by frequency.
#' Default is FALSE.
#' @param .quiet A logical value.
#' If TRUE, do not print the result. Default is FALSE.
#'
#' @return
#' An object of the same type as `.data`.
#'
#' @name tab
#'
#' @examples
#' # tab() the 8th column
#' tab(starwars, 8)
#' tab(starwars, sex)
#' tab(starwars, sex, sort = TRUE)
#' tab(starwars, sex, gender)
#' tab(starwars, gender, sex)
NULL

#' @export
#' @rdname tab
tab <- function(
    .data, ...,
    sort = FALSE,
    .print = TRUE,
    .print_n = 10) {
  if (!missing(...)) {
    select_works <- try_success(
      dplyr::select(.data, ...)
    )
    if (select_works) {
      .data <- dplyr::select(
        .data,
        ...
      )
    } else {
      .data <- dplyr::transmute(
        .data,
        ...
      )
    }
  } else {
    stop("Please provide the variable to tabulate.")
  }

  if (ncol(.data) > 1) {
    stop("Please use `fre()`, `tab1()` or `tab2()` for tabulating multiple variables.")
  }

  varname <- names(.data)
  out <- .data %>%
    dplyr::rename(value = 1) %>%
    dplyr::count(value)
  out <- dplyr::mutate(out, percent = n / sum(n) * 100)
  if (sort) {
    out <- dplyr::arrange(out, -percent)
  }
  out <- out %>%
    dplyr::mutate(
      value = value %>%
        as_character(),
      cum = cumsum(percent)
    ) %>%
    dplyr::add_row(
      value = "total",
      n = dplyr::pull(out, n) %>% sum(),
      percent = 100
    ) %>%
    dplyr::rename(
      {{ varname }} := value
    )

  if (.print) {
    out %>%
      head_tail_print(
        n = .print_n,
        .tail_row = TRUE
      )
  }

  return(invisible(out))
}

#' @export
#' @rdname tab
tab1 <- function(
    .data, ...,
    sort = FALSE,
    .separate = FALSE,
    .print = TRUE,
    .print_n = 10) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    select_works <- try_success(
      dplyr::select(.data, ...)
    )
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

  out <- .data %>%
    purrr::map(
      ~ tibble::tibble(value = .)
    ) %>%
    purrr::map(
      ~ tab(
        ., value,
        sort = sort,
        .print = FALSE
      )
    )

  if (!.separate) {
    out <- out %>%
      purrr::imap(
        ~ dplyr::mutate(.x, variable = .y)
      ) %>%
      purrr::list_rbind() %>%
      dplyr::relocate(variable)
  }

  if (.print) {
    out %>%
      head_tail_print(
        n = .print_n,
        .tail_row = TRUE
      )
  }

  return(invisible(out))
}

#' @export
#' @rdname tab
tab2 <- function(
    .data,
    row_var, col_var,
    .print = TRUE, .print_n = 10, .print_width = NULL) {
  if (missing(col_var)) {
    warning("Please provide the second variable.
    `tab()` is recommended for tabulating a single variable.")
    out <- .data %>%
      dplyr::count(
        value = {{ row_var }}
      )
    out <- dplyr::mutate(out, percent = n / sum(n) * 100)
    if (sort) {
      out <- dplyr::arrange(out, -percent)
    }
    out <- out %>%
      dplyr::mutate(
        value = value %>%
          as_character(),
        cum = cumsum(percent)
      ) %>%
      dplyr::add_row(
        value = "total",
        n = dplyr::pull(out, n) %>% sum(),
        percent = 100
      ) %>%
      dplyr::rename(
        {{ row_var }} := value
      )
  } else {
    out <- dplyr::count(
      .data, {{ row_var }}, {{ col_var }}
    ) %>%
      tidyr::pivot_wider(
        names_from = {{ col_var }},
        values_from = n
      ) %>%
      dplyr::mutate(
        dplyr::across(-1, ~ replace_na(.x, 0)),
        {{ row_var }} := {{ row_var }} %>%
          as_character()
      ) # replace NAs with 0s
    out_sum <- out %>%
      dplyr::mutate(
        dplyr::across(-1, ~ sum(out$.)),
        {{ row_var }} := "total"
      ) %>%
      dplyr::slice(1)
    out <- out %>%
      dplyr::bind_rows(out_sum) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        total = dplyr::c_across(-1) %>%
          sum()
      ) %>%
      dplyr::ungroup()
  }

  if (.print) {
    out %>%
      head_tail_print(
        n = .print_n,
        width = .print_width,
        .tail_row = TRUE
      )
  }

  return(invisible(out))
}


#' @export
#' @rdname tab
fre <- function(
    .data, ..., sort = FALSE,
    .print = TRUE, .print_n = 10) {
  # if ... is empty, select all variables
  if (!missing(...)) {
    select_works <- try_success(
      dplyr::select(.data, ...)
    )
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

  ncols <- .data %>% ncol()
  out <- .data %>%
    dplyr::count(
      dplyr::across(
        dplyr::everything()
      )
    ) %>%
    dplyr::mutate(
      percent = n / sum(n) * 100,
      sum_valid = dplyr::if_else(
        dplyr::if_any(
          dplyr::all_of(1:ncols), ~ is.na(.)
        ), 0, percent
      ) %>% sum(),
      valid = if_else(
        dplyr::if_any(
          dplyr::all_of(1:ncols), ~ is.na(.)
        ), NA_real_, percent / sum_valid * 100
      )
    )
  if (sort) {
    out <- dplyr::arrange(out, -valid)
  }
  out <- out %>%
    dplyr::mutate(cum = cumsum(valid)) %>%
    dplyr::select(-sum_valid)

  if (.print) {
    out %>%
      head_tail_print(
        n = .print_n,
        .tail_row = TRUE
      )
  }

  return(invisible(out))
}
