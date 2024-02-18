try_success <- function(f) {
  try_result <- try(f, TRUE)
  try_success <- !inherits(try_result, "try-error")
  return(try_success)
}
