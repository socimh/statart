#' Try a function and return a logical
#' 
#' This function tries to evaluate a function, 
#' and if the function throws an error, it returns FALSE.
#' Otherwise, it returns TRUE.
#' 
#' @param .fn A function call.
#' @returns A logical value.
#'
#' @export
#' @examples
#' s_try(mean(1:10))
#' s_try(mean(letters[1:10]))
s_try <- function(.fn) {
  try_result <- try(.fn, TRUE)
  s_try <- !inherits(try_result, "try-error")
  return(s_try)
}
