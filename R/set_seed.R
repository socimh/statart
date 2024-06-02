#' Set seeds with pipeline.
#'
#' Attach a seed to a specific function.
#'
#' @param .fn A function call.
#' @param seed An integer for reproducibility.
#' @returns Return the function result with a seed set.
#'
#' @export
#' @examples 
#' sample(1:10, 8) %>% 
#'   set_seed(123)
#' 
#' # When we re-run the code, we will get the same result.
#' sample(1:10, 8) %>%
#'  set_seed(123)
set_seed <- function(.fn, seed) {
  set.seed(seed)
  return(.fn)
}