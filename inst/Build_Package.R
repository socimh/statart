pacman::p_load(devtools, usethis, tidyverse, sloop, pkgdown, cpp11)
build_readme()

# usethis::use_pkgdown_github_pages()
use_pkgdown()
# VPN set to 美国
pkgdown::build_site()
preview_site()

devtools::install_github("socimh/statart")
library(statart)
sessionInfo()
library(statart)
ls("package:statart")

remotes::install_deps(dependencies = TRUE)

library(pkgbuild)
library(cpp11)
compile_dll(force = TRUE, compile_attributes = pkg_links_to_cpp11("C:/Users/socim/AppData/Local/R/win-library/4.3/cpp11"))

usethis::use_cpp11()
devtools::document()
devtools::load_all()
cpp11::cpp_register()

vec <- 1:5e7 %>% as.double()
s_max(vec)
n_distinct10k(vec) %>% s_time()
n_distinct(vec) %>% s_time()
row_sum(tb)

aes_to_tbl() %>%
  print(n = 100)

tb <- tibble(
  x = 1:5,
  y = 2:6,
  z = 11:15
) %>%
  mutate(across(
    everything(),
    as.double
  ))
row_unique(tb)
row_min(tb)
row_max(tb)
row_sum(tb)

# cpp_source("D:/R/statart/src/sum.cpp")

row_sum(tb)[1:5]
dbl_sd(1:1e7 %>% as.double())
n_distinct(1:1e7 %>% as.double())

tb <- tibble(
  x1 = runif(1e4),
  x2 = runif(1e4),
  x3 = runif(1e4),
  x4 = runif(1e4),
  x5 = runif(1e4),
  x6 = runif(1e4),
  x7 = runif(1e4),
  x8 = runif(1e4),
  x9 = runif(1e4),
  x10 = runif(1e4)
)

cpp_source("D:/R/statart/src/sum.cpp")
s_sum(vec)

tb %>% row_sum() %>% head()
tb %>%
  row_sum2() %>%
  head()

tb %>% rowSums() %>% head()

bench::mark(
  f1 = tb %>% mutate(total = rowSums(pick(starts_with("x")))) %>% nrow(),
  f3 = tb %>% mutate(total = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) %>% nrow(),
  f4 = tb %>% mutate(total = row_sum(pick(starts_with("x")))) %>% nrow(),
  f5 = tb %>%
    mutate(total = row_sum2(pick(starts_with("x")))) %>%
    nrow()
) %>%
  arrange(median)

bench::mark(
  f1 = tb %>% mutate(total = rowSums(pick(starts_with("x")))),
  f2 = tb %>% mutate(total = pmap_dbl(pick(starts_with("x")), sum)),
  f3 = tb %>% mutate(total = x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10),
  f4 = tb %>% mutate(total = row_sum(pick(starts_with("x")))),
  f5 = tb %>% mutate(total = row_sum2(pick(starts_with("x"))))
) %>%
  arrange(median)

bench::mark(
  pmax = tb %>% mutate(total = pmax(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)),
  pmap = tb %>% mutate(total = pmap_dbl(pick(starts_with("x")), max)),
  row_max = tb %>% mutate(total = row_max(pick(starts_with("x")))),
  row_max2 = tb %>% mutate(total = row_max2(pick(starts_with("x"))))
) %>%
  arrange(median)

tb2 <- tb %>% head(1e3)
bench::mark(
  pmax = tb2 %>% mutate(total = pmax(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)),
  pmap = tb2 %>% mutate(total = pmap_dbl(pick(starts_with("x")), max)),
  # very slow & memory intensive
  c_across = tb2 %>% rowwise() %>% mutate(total = c_across(starts_with("x")) %>% max()) %>% ungroup(),
  row_max = tb2 %>% mutate(total = row_max(pick(starts_with("x")))),
  row_max2 = tb2 %>% mutate(total = row_max2(pick(starts_with("x"))))
) %>%
  arrange(median)



vec_tb1 <- runif(1e6) %>%
  c(rep(NA_real_, 1e6)) %>%
  tibble(a = .)
vec_tb2 <- runif(2e6) %>% tibble(a = .)
bench::mark(
  f1a = vec_tb1 %>% summarise(mean = mean(a, na.rm = TRUE)) %>% nrow(),
  f1b = vec_tb2 %>% summarise(mean = mean(a, na.rm = TRUE)) %>% nrow(),
  f2a = vec_tb1 %>% summarise(mean = mean(a)) %>% nrow(),
  f2b = vec_tb2 %>% summarise(mean = mean(a)) %>% nrow(),
  f3a = vec_tb1 %>% summarise(mean = s_mean(a)) %>% nrow(),
  f3b = vec_tb2 %>% summarise(mean = s_mean(a)) %>% nrow()
) %>%
  arrange(median)

vec_tb <- runif(1e6) %>%
  c(rep(NA_real_, 1e6)) %>%
  tibble(a = .)
bench::mark(
  f1a = vec_tb %>% summarise(mean = mean(a, na.rm = TRUE)) %>% nrow(),
  f2b = vec_tb %>% summarise(mean = mean(a)) %>% nrow(),
  f3a = vec_tb %>% summarise(mean = s_mean(a)) %>% nrow()
) %>%
  arrange(median)



vec <- sample(1:20, 1e4, replace = TRUE) %>% as.double()
s_mean(c(1:4, NA_integer_, NA_integer_) %>% as.double())
s_median(1:(1e5 + 1) %>% as.double())
s_median(1:4 %>% as.double())
s_unique2(c(1:5, 5, 1, 1, 1:5) %>% as.double())

# cpp_source("D:/R/statart/src/sum.cpp")
bench::mark(
  mean(vec) %>% length(),
  sum(vec) %>% length(),
  s_sum(vec) %>% length(),
  s_mean(vec) %>% length(),
  s_max(vec) %>% length(),
  dbl_sd(vec) %>% length(),
  n_distinct10k(vec) %>% length(),
  sd(vec) %>% length(),
  min(vec) %>% length(),
  max(vec) %>% length(),
  length(vec) %>% length(),
  n_distinct(vec) %>% length(),
  median(vec) %>% length(),
  s_median(vec) %>% length()
) %>%
  arrange(median)



vec_tb <- tibble(
  vec1 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec2 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec3 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec4 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec5 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec6 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec7 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec8 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec9 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double(),
  vec10 = sample(1:1e4, 1e5, replace = TRUE) %>% as.double()
)

mres <- bench::mark(
  f1 = vec_tb %>%
    summarise(
      across(
        everything(),
        n_distinct10k
      )
    ) %>% nrow(),
  f2 = vec_tb %>%
    summarise(
      across(
        everything(),
        n_distinct
      )
    ) %>% nrow()
) %>%
  arrange(median)
mres %>%
  print(width = Inf)


tibble(
  x = runif(1e4),
  y = runif(1e4),
  z = runif(1e4)
) %>%
  mutate(total = rowSums(pick(x, y, z))) %>%
  s_time()


# add works like a regular R function
add
add(1, 2, 3)

x <- runif(1e8)
y <- runif(1e8)

pmax(x, y) %>%
  s_time()
psum(x, y) %>%
  s_time()

# 0.04 secs
tibble(x, y) %>%
  mutate(max = pmax(x, y)) %>%
  s_print() %>%
  s_time()

# 0.03 secs
tibble(x, y) %>%
  mutate(sum = psum(x, y)) %>%
  s_print() %>%
  s_time()





# 43 secs
tb %>%
  rowwise() %>%
  mutate(temp = sum(c_across(ends_with("temp")), na.rm = TRUE)) %>%
  s_time()

# 0.005 secs
tb %>%
  mutate(temp = rowSums(pick(ends_with("temp")))) %>%
  select(ends_with("temp")) %>%
  s_time()

# 0.005 secs
tb %>%
  mutate(temp = pmax(pick(ends_with("temp")))) %>%
  select(ends_with("temp")) %>%
  s_time()

# 0.196 secs
tb %>%
  mutate(temp = pmap_dbl(pick(ends_with("temp")), max)) %>%
  select(ends_with("temp")) %>%
  s_time()

?rowSums



load_all()
tibble(x = c(1:100, 190:200)) %>%
  s_plot(1, x, geom = c("violin", "boxplot")) %>%
  s_ggsave()

tibble(x = c(1:100, 190:200)) %>%
  ggplot() +
  art_violin(aes(1, x))


gg <- ggplot(iris, aes(Species, Sepal.Length)) +
  geom_violinbox()
s_ggsave(gg)

iris
gg <- ggplot(iris, aes(Species, Sepal.Width)) +
  geom_violinbox()
s_ggsave(gg, "plot2.png")

load_all()
starwars %>%
  filter(mass < 500) %>%
  s_plot(height, mass) %>%
  s_ggsave("plot.png")

plot_tb <- tibble(
  x = sample(1:8, 5e3, prob = 1:8, replace = TRUE),
  y = sample(1:8, 5e3, prob = 1:8, replace = TRUE)
)

load_all()
plot_tb %>%
  s_plot(x, y) %>%
  s_ggsave("plot.png")

load_all()
gg <- tibble(
  x = sample(c(T, F), 1e2, replace = TRUE, prob = c(.3, .7))
) %>%
  s_plot(x) +
  geom_text(aes(x, after_stat(count) * .5, label = after_stat(count)), stat = "count")
s_ggsave(gg, "plot.png")

# Labelling the upper hinge of a boxplot,
# inspired by June Choe
# Labelling a bar plot
ggplot(mpg, aes(class)) +
  geom_bar() +
  geom_text(
    aes(
      y = after_stat(count + 2),
      label = after_stat(count)
    ),
    stat = "count"
  )

vec <- 1:5 * 10
s_type(vec)

load_all()
plot_tb %>%
  s_plot(factor(x), factor(y)) %>%
  s_ggsave("plot.png")

plot_tb %>%
  s_plot(factor(x), y) %>%
  s_ggsave("plot.png")


lifeexp %>%
  ggplot() +
  geom_bar(aes(x = region))

lifeexp %>%
  s_plot(gnppc, lexp, fill = as_character(region)) %>%
  s_ggsave()

ggplot(starwars) +
  geom_smooth(aes(x = height, y = mass), method = "lm")

starwars %>%
  filter(mass < 500) %>%
  s_plot(height, mass, geom = c("smooth", "point"))
starwars %>%
  filter(mass < 500) %>%
  s_plot(height, mass, geom = c("point", "smooth"))

document()
getwd()
load_all()
check()


lifeexp
load_all()
# change_ggplot_style()
s_plot(lifeexp, region, NULL)
s_plot(lifeexp, NULL, region)
s_plot(lifeexp, factor(region))
s_plot(lifeexp, country)
s_plot(lifeexp, country, region)
s_plot(lifeexp, region, country)
s_plot(lifeexp, popgrowth)
s_plot(lifeexp, safewater)
s_plot(data = lifeexp, gnppc, lexp, fill = as_character(region))
s_plot(data = lifeexp, gnppc, lexp, fill = seq_along(country))
s_plot(lifeexp, gnppc, lexp) +
  scale_x_log10()


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


s3_methods_generic("tbl_sum")
s3_methods_generic("ctl_new_rowid_pillar")
s3_methods_generic("tbl_format_footer")

s3_methods_class("head_tail")
