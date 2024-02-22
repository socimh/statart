#' Try a function and return a logical
#'
#' @param .fn A function call.
#' @returns A logical value.
#'
#' @export
s_try <- function(.fn) {
  try_result <- try(.fn, TRUE)
  s_try <- !inherits(try_result, "try-error")
  return(s_try)
}
