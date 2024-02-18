pacman::p_load(devtools, usethis)

getwd()
create_package(getwd())
use_git()

devtools::dev_sitrep()
devtools::install_dev_deps()

available::available("statart")
pak::pkg_name_check("statart")

use_r("get_type_abbr")
use_import_from("rlang", ":=")
starwars <- dplyr::starwars
use_data(starwars)
# import lifeexp.dta from Stata
# lifeexp <- haven::read_dta("lifeexp.dta")
# use_data(lifeexp)

load_all()
test()
get_type_abbr(1:10)
ls("package:statart")
check()

test()

pacman::p_load(devtools, usethis, tidyverse)
document()
install(upgrade = FALSE)
library(statart)
ls("package:dplyr")

lifeexp %>%
  mutate(
    region2 = haven::as_factor(region),
    .keep = "used"
  ) %>%
  tab1()

starwars %>%
  codebook()

lifeexp %>%
  tab(as.numeric(region))

lifeexp %>%
  tab1(region, .desc = TRUE)

lifeexp %>%
  tab1(region:country, .desc = TRUE)
lifeexp %>%
  fre(region, .desc = TRUE)

document()
load_all()
?tab
lifeexp %>%
  tab(region)
lifeexp %>%
  tab(starts_with("region"))
lifeexp %>%
  tab(region == 1)
lifeexp %>%
  tab(eurasia = region == 1)
starwars %>%
  tab(films)

starwars %>%
  fre()

document()
load_all()

starwars %>%
  head_tail_print()

starwars %>%
  arrange(-height) %>%
  head_tail_print()

tab(lifeexp, region)
tab(lifeexp, country)

tab1(lifeexp)

load_all()
lifeexp %>%
  tab1(region)
lifeexp %>%
  tab1(region:country)

lifeexp %>%
  tab1(region, as.numeric(region))
lifeexp %>%
  tab1(
    region, as.numeric(region),
    .separate = TRUE
  )




# Example usage
result <- try_function(log("a"))
print(result)

starwars %>%
  tab1()
fre() %>%
  select(n:cum)

starwars %>%
  purrr::map(
    tibble
  )

numeric <- function(...) {
  dots <- list(...)
  dots
}

numeric(1, 2, 3)


attrs <- list(x = 1, y = 2)
attr_name <- "z"

1:10 %>%
  set_attr(w ~ 0, x = 2)
set_attr(w = 0, !!!attrs, !!attr_name := 3)

testit <- function(.data, row_var) {
  .data <- .data %>%
    dplyr::mutate(
      row_var
    )
  .data
}
?recode

testit(lifeexp, haven::as_factor(region))

lifeexp %>%
  mutate(
    a = haven::as_factor(region) %>%
      as.character()
  )

?on.exit
use_github()

use_data_raw("complex_tb")

check()
