## code to prepare `complex_tb` dataset goes here
complex_tb <- tibble(
  date = as.Date("2024-02-09") - 1:1000,
  time = as_datetime("2024-02-09 12:00:00") - 1:1000,
  duration1 = as_datetime("2024-02-09 12:00:00") -
    as_datetime("2024-02-09 10:00:00") + 1:1000,
  duration2 = hms::as_hms("12:34:56"),
  string = "ABC",
  logical = rep(c(TRUE, FALSE), 500),
  unit1 = units::set_units(1:1000, "m"),
  unit2 = units::set_units(1:1000, "m^2"),
  factor = factor(rep(letters[1:2], 500)),
  order = factor(rep(letters[1:2], 500),
    ordered = TRUE
  ),
  double = 1:1000 / 10,
  integer = 1:1000L,
  half_miss = if_else(
    1:1000 %% 2 == 1,
    1:1000, NA_real_
  ),
  all_miss = NA_real_
)

usethis::use_data(complex_tb, overwrite = TRUE)
