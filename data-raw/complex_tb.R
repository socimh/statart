## code to prepare `complex_tb` dataset goes here
complex_tb <- tibble(
  date1 = as.Date("2024-01-01") - 1:1000,
  date2 = as.Date("2024-01-01") + 1:1000,
  time = as_datetime("2024-02-09 12:00:00") - 1:1000,
  datetime = as_datetime("2024-02-09 12:00:00") -
    as_datetime("2024-02-09 10:00:00") + 1:1000,
  hmstime = hms::as_hms("12:34:56"),
  string = "ABC",
  logical = rep(c(TRUE, FALSE), 500),
  unit_m = units::set_units(1:1000, "m"),
  unit_m2 = units::set_units(1:1000, "m^2"),
  labelled_int = haven::labelled(
    rep(1:2, 500),
    c("Apple" = 1, "Banana" = 2)
  ),
  labelled_dbl = haven::labelled(
    c(rep(c(1.0, 2.0), 500)),
    c("Apple" = 1, "Banana" = 2)
  ),
  labelled_chr = haven::labelled(
    rep(letters[1:2], 500),
    c("Apple" = "a", "Banana" = "b")
  ),
  factor = factor(rep(letters[1:2], 500)),
  order = factor(
    rep(letters[1:2], 500),
    ordered = TRUE
  ),
  double = 1:1000 / 10,
  integer = 1:1000L,
  half_miss = if_else(
    1:1000 %% 2 == 1,
    1:1000, NA_real_
  ),
  all_miss1 = NA,
  all_miss2 = NA_integer_
)
