pacman::p_load(devtools, usethis, tidyverse, sloop)
build_readme()

document()
getwd()
create_package(getwd())
use_git()
use_pipe()
load_all()
check()

unclass(haven::labelled(1:10, c(Bad = 1, Good = 5))) %>%
  type_sum()

lifeexp %>%
     dplyr::mutate(
       region_chr = as_character(region),
       region_num = as_numeric(region),
       region_fct = haven::as_factor(region),
       .keep = "used"
     )
s_print(lifeexp)
lifeexp %>%
  mutate(
    region2 = haven::as_factor(region)
  ) %>%
  summ()
starwars %>% summ()
case_match()
devtools::dev_sitrep()
devtools::install_dev_deps()

available::available("statart")
pak::pkg_name_check("statart")

ls("package:statart")
load_all()

lifeexp %>%
  fre() %>%
  s_print(n = 20)
fre1(complex_tb) %>%
  walk(s_print)


skimr::skim(lifeexp)
skimr::skim(complex_tb)
s3_dispatch(s_type(lifeexp))

s3_methods_generic("mutate")
s3_methods_generic("select")

s3_methods_class("ordered")
s3_methods_class("data.frame")

tb <- class_tbl(lifeexp, "head_tail", 5)

tb
s3_methods_generic(class_tbl(lifeexp, "head_tail", 5))
s3_methods_generic("class_tbl")
s3_class(tb)
str(tb)
unclass(tb)
tb


## Create an object inheriting from class 'cls', and call the
## generic on it:
type_sum(tb)
tb

document()
install(upgrade = FALSE)
load_all()
library(sloop)
?tbl_sum
s3_methods_generic("tbl_sum")
s3_methods_generic("ctl_new_rowid_pillar")
s3_methods_generic("tbl_format_footer")

s3_methods_class("head_tail")

s_print(lifeexp)
s_print(starwars)
print(starwars)

tb <- class_tbl(lifeexp, "head_tail", 5)
s3_class(tb)
tbl_sum(tb)
ctl_new_rowid_pillar(tb)
tbl_format_footer(tb)
tb
as(arrange(mtcars, cyl), "DataFrame")

library(haven)
x <- labelled(sample(5, 10, replace = TRUE), c(Bad = 1, Good = 5))
as.numeric(x)
as_factor(x) %>%
  as.numeric()


s_print(starwars)
print(class_tbl(lifeexp, "head_tail", 5))

s3_dispatch(tbl_sum(tb))
s3_dispatch(ctl_new_rowid_pillar(tb))
s3_dispatch(class_tbl(lifeexp, "head_tail", 5))
s3_dispatch(print(tb))


?print.tbl


lifeexp %>%
  fre(safewater) %>%
  s_print(n = 8)

lifeexp %>%
  fre(safewater) %>%
  s_print(n = 8, .append = FALSE)



use_r("s_type")
use_import_from("rlang", ":=")
starwars <- dplyr::starwars
use_data(starwars)
# import lifeexp.dta from Stata
# lifeexp <- haven::read_dta("lifeexp.dta")
# use_data(lifeexp)

load_all()
test()
s_type(1:10)
ls("package:statart")
check()

test()

pacman::p_load(devtools, usethis, tidyverse)
document()
install(upgrade = FALSE)
library(statart)
ls("package:statart")
?starwars

lifeexp %>%
  large_starwars() <- starwars %>%
  sample_n(1e6, replace = TRUE) %>%
  s_time()

load_all()
?fre
large_starwars %>%
  summ() %>%
  s_time()

large_starwars %>%
  summarise(summ_var(height)) %>%
  s_time()

large_starwars %>%
  summarise(summ_var(height, stat = "mean")) %>%
  s_time()
large_starwars %>%
  summarise(height %>% mean(na.rm = T)) %>%
  s_time()
large_starwars %>%
  summarise(
    across(height, ~ tibble(mean = mean(.x, na.rm = TRUE)))
  ) %>%
  s_time()

use_github_action()
use_github_action("check-standard")
usethis::use_lifecycle()
load_all()
use_version()
check()

document()
load_all()
edit_r_environ()

lifeexp %>%
  mutate(
    region2 = haven::as_factor(region),
    .keep = "used"
  ) %>%
  tab1(.append = TRUE) %>%
  tab()

load_all()


load_all()
starwars %>%
  tab1(gender, sex)
starwars %>%
  tab1(gender, birth_year)
starwars %>%
  tab1(gender, birth_year, .append = TRUE)
starwars %>%
  tab1(.append = TRUE)

pacman::p_load(devtools, usethis, tidyverse)
document()
load_all()
install(upgrade = FALSE)
?tab
starwars %>%
  tab2(s_match("s*e"))
starwars %>%
  tab2(s_match("s*e"),
    .flip = TRUE
  ) %>%
  s_print(n = 5)

starwars %>%
  tab2(birth_year, sex)
starwars %>%
  fre2(birth_year, sex) %>%
  s_print()

starwars %>%
  fre2(
    s_match("s*e"),
    .flip = TRUE
  ) %>%
  s_print()


starwars %>%
  tab2(gender, young = birth_year > 50)
starwars %>%
  tab2(young = birth_year > 50, gender)

lifeexp %>%
  mutate(
    gnppc = if_else(
      gnppc < 400, 2, gnppc
    )
  ) %>%
  tab1(gnppc, region)

lifeexp %>%
  fre(country, safewater)

lifeexp %>%
  tab(region, gnppc)
lifeexp %>%
  group_by(region) %>%
  tab0(mean(gnppc))

vector1 <- c("a", "b", "c")
vector2 <- c("c", "d")
intersect(vector1, vector2)

load_all()
lifeexp %>%
  tab_data0(as.numeric(region))

lifeexp %>%
  tab1(region, .desc = TRUE)

lifeexp %>%
  tab1(region:country, .desc = TRUE)
lifeexp %>%
  tab_data0(safewater)

document()
load_all()
?tab

lifeexp %>%
  tab(region, safewater)

load_all()
starwars %>%
  fre(sex == "male")
starwars %>%
  fre(sex, birth_year)

starwars %>%
  tab1()

starwars %>%
  fre()

document()
load_all()

starwars %>%
  s_print()

starwars %>%
  arrange(-height) %>%
  s_print()

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
