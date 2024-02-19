#' @export 
s_matches <- function(chr) {
  if (is.character(chr)) {
    out <- paste0("^(", chr, "*)$")
    out <- stringr::str_replace_all(out, "\\*", ".*")
    out <- stringr::str_replace_all(out, "\\s+", ".*|")
    out <- stringr::str_replace_all(out, "(\\.\\*){2}", ".*")
  }
  return(tidyselect::matches(out))
}