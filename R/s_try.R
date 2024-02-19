s_try <- function(f) {
  try_result <- try(f, TRUE)
  s_try <- !inherits(try_result, "try-error")
  return(s_try)
}
