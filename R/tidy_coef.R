#' Directly obtain the regression results
#'
#' @description
#' Given a model fit object (e.g. from `lm()` or `glm()`), `tidy_coef()` returns a tibble of coefficients.
#'
#' @inheritParams regress
#'
#' @return Return the regression results as a list or a tibble.
#'
#' @seealso [regress()]
#'
#' @export
#' @examples
#' lm(mpg ~ wt + hp, data = mtcars) %>%
#'   tidy_coef()
tidy_coef <- function(
    model,
    digit = integer(0),
    format = "%.3f") {
  broom_works <- s_try(broom::tidy(model))
  broom_mixed_works <- s_try(broom.mixed::tidy(model))

  if (broom_works) {
    out <- broom::tidy(model)
  } else if (broom_mixed_works) {
    nobs <- broom.mixed::glance(model) %>%
      dplyr::pull(df.residual)
    out <- broom.mixed::tidy(model) %>%
      dplyr::mutate(
        p.value = 2 * pt(q = statistic, df = nobs - 1, lower.tail = FALSE)
      )
  } else {
    stop("tidy() does not work for the model.")
  }

  if (length(digit) > 1) {
    stop("The digit should be a positive integer.")
  } else if (length(digit) == 1) {
    digit <- as.integer(digit)
    format <- stringr::str_glue(
      "%.{digit}f"
    )
  }

  out <- out %>%
    dplyr::mutate(
      star = dplyr::case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01 ~ "**",
        p.value < 0.05 ~ "*",
        p.value < 0.1 ~ "+",
        TRUE ~ ""
      ),
      coef = sprintf(format, estimate),
      coef = ifelse(
        is.na(std.error), coef,
        stringr::str_glue(
          "{coef}{star}\n({se})",
          se = sprintf(format, std.error)
        )
      )
    ) %>%
    dplyr::relocate(coef, .after = term)

  return(out)
}
