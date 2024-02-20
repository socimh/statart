#' Enhanced select() with data-masking
#' 
#' @inheritParams tab
#' 
#' @return A tibble
#' @export
#' 
#' @examples
#' starwars %>%
#'  s_select()
#' starwars %>%
#'  s_select(name, height)
#' \dontrun{
#' starwars %>%
#'  dplyr::select(height > 50)
#' }
#' starwars %>%
#'  s_select(height > 50)
s_select <- function(.data, ...) {
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
