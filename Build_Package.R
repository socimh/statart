
library(devtools)
packageVersion("devtools")

getwd()
create_tidy_package(getwd())

use_r("get_unit")
use_r("get_type_abbr")
use_r("read_var_type")
use_r("codebook")

load_all()

library(tidyverse)
tibble(
  a = 1:10, 
  b = letters[1:10]
) %>%
  codebook()

