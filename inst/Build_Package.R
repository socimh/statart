
pacman::p_load(devtools, usethis)

getwd()
create_tidy_package(getwd())

# use_git()

use_r("get_unit")
use_r("get_type_abbr")
use_r("read_var_type")
use_r("codebook")

use_data_raw("complex_tb")

document()

load_all()
check()

install()

library(statart)
get_type_abbr(1:10)
?statart::get_type_abbr
ls("package:statart")

codebook(tibble::tibble(x = 1:10, y = letters[1:10]))

use_github()

devtools::install_github("socimh/statart")

devtools::build_readme()

?install_github

??statart

