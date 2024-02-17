pacman::p_load(devtools, usethis)

getwd()
create_package(getwd())
use_git()

devtools::dev_sitrep()
devtools::install_dev_deps()

available::available("statart")
pak::pkg_name_check("statart")

use_r("get_type_abbr")
use_pipe()
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

document()
install(upgrade = FALSE)
library(statart)
ls("package:statart")
??haven

use_github()

use_data_raw("complex_tb")

check()
