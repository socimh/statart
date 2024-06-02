#' Set seeds with pipeline.
#'
#' Attach a seed to a specific function.
#'
#' @param .fn A function call.
#' @param seed An integer for reproducibility.
#' @returns Return the function result with a seed set.
#'
#' @export
set_seed <- function(.fn, seed) {
  set.seed(seed)
  return(.fn)
}