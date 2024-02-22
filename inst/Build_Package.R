pacman::p_load(devtools, usethis)
library(tidyverse)
build_readme()

document()
getwd()
create_package(getwd())
use_git()
use_pipe()

devtools::dev_sitrep()
devtools::install_dev_deps()

available::available("statart")
pak::pkg_name_check("statart")

ls("package:statart")
load_all()
s_print(lifeexp)

skimr::skim(lifeexp)
library(sloop)
s3_dispatch(s_type(lifeexp))

s3_methods_generic("mutate")
s3_methods_generic("select")

s3_methods_class("ordered")
s3_methods_class("data.frame")

tb <- class_tbl(lifeexp, "head_tail", 5)
attr(tb, "row.names") <- attr(tb, "rows")
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
load_all()
s3_methods_generic("tbl_sum")
s3_methods_generic("ctl_new_rowid_pillar")
s3_methods_generic("print") %>%
  filter(str_detect(class, "^he"))

s3_methods_class("head_tail")

tb <- class_tbl(lifeexp, "head_tail", 5)
s3_class(tb)
tbl_sum(tb)
ctl_new_rowid_pillar(tb)
tb

source("https://install-github.me/nbenn/prt")

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

s_print2(lifeexp, "head_tail")

class_tbl <- function(.data, class, n) {
  vctrs::new_data_frame(
    dplyr::bind_rows(
      .data %>% head(n),
      .data %>% tail(n)
    ),
    nrow = nrow(.data),
    rows = c(
      seq_len(n),
      seq(nrow(.data) - n + 1, nrow(.data))
    ),
    class = c(class, "tbl")
  )
}

tbl_sum.head_tail <- function(x, ...) {
  c("A tibble" = paste0(
    attr(x, "nrow"),
    " × ",
    ncol(x)
  ))
}

ctl_new_rowid_pillar.head_tail <- function(controller, x, width, ...) {
  out <- NextMethod()
  rowid <- attr(controller, "rows")
  width <- max(nchar(as.character(rowid)))
  list(
    title = out$title,
    type = out$type,
    data = pillar::new_pillar_shaft(
      list(row_ids = rowid),
      width = width,
      class = "pillar_rif_shaft"
    ) %>%
      pillar::pillar_component()
  ) %>%
    pillar::new_pillar(width = width)
}

tbl_format_footer.head_tail <- function(x, setup, ...) {
  paste0(
    "Notice: Middle ", attr(x, "nrow") - length(attr(x, "rows")), " rows are hidden."
  )
}

class_tbl(cbind(lifeexp, lifeexp), "head_tail", 5) %>%
  print(width = Inf)




tbl_format_header.header <- function(x, setup, ...) {
  pillar::style_subtle("Head and Tail")
}

class_tbl(lifeexp, "header", 5)



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
