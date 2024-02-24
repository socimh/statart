pacman::p_load(devtools, usethis, tidyverse, sloop, pkgdown)
build_readme()

# usethis::use_pkgdown_github_pages()
use_pkgdown()
# VPN set to 美国
pkgdown::build_site()

devtools::install_github("socimh/statart")
library(statart)
sessionInfo()
library(statart)
?statart
ls("package:statart")

document()
getwd()
load_all()
check()



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


