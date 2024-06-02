#' Time spent on a function
#'
#' @param .x The function
#' @return The function output
#' @export
#'
#' @examples
#' x <- seq_len(1e6)
#' mean(x) %>% s_time()
s_time <- function(.x) {
  begin_time <- Sys.time()
  out <- .x
  end_time <- Sys.time()
  time <- end_time - begin_time
  duration_to_chr(time) %>%
    paste0("Time spent: ", .) %>%
    message()
  invisible(out)
}

duration_to_chr <- function(duration) {
  time_list <- duration %>%
    hms::as_hms() %>%
    as.character() %>%
    stringr::str_split_1(":")

  h_num <- time_list[[1]] %>%
    as.numeric()
  time_list[[1]] <- dplyr::case_match(
    time_list[[1]],
    "00" ~ "",
    "01" ~ "1 hour",
    .default = paste0(time_list[[1]], " hours")
  ) %>%
    stringr::str_remove("^0*")

  m_num <- time_list[[2]] %>%
    as.numeric()
  time_list[[2]] <- dplyr::case_match(
    time_list[[2]],
    "00" ~ "00 min",
    "01" ~ "10 min",
    .default = paste0(time_list[[2]], " mins")
  )

  secs <- dplyr::if_else(
    h_num + m_num == 0,
    time_list[[3]] %>%
      as.numeric(),
    time_list[[3]] %>%
      as.numeric() %>%
      +(100)
  )

  secs_test <- sprintf("%.3g", secs)

  time_list[[3]] <- dplyr::case_when(
    stringr::str_detect(secs_test, "^1\\d{2}$") ~
      stringr::str_remove(secs_test, "^1"),
    stringr::str_length(secs_test) < 3 ~
      paste0(secs_test, ".00"),
    stringr::str_detect(secs_test, "^0\\.") &
      stringr::str_length(secs_test) < 5 ~
      sprintf("%.3f", secs),
    stringr::str_detect(secs_test, "e") ~
      sprintf("%.3f", secs),
    stringr::str_length(secs_test) > 5 ~
      sprintf("%.3f", secs),
    TRUE ~ secs_test
  ) %>%
    paste0(" secs")

  out <- time_list %>%
    stringr::str_flatten(" ") %>%
    stringr::str_remove("^\\s*") %>%
    stringr::str_remove("^00 min ")

  return(out)
}
