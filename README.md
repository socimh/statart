
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statart

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/statart)](https://CRAN.R-project.org/package=statart)
[![R-CMD-check](https://github.com/socimh/statart/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/socimh/statart/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

The primary goal of statart is to migrate some useful commands
(functions) from Stata to R. In terms of certain jobs, however,
functions in `statart` may perform better than any existing functions in
Stata or R.

These functions are built in the tidyverse framework, including

- `codebook()` for codebook (like `codebook` in Stata)
- `summ()` for summary statistics (like `summarize` in Stata)
- `tab()`, `tab1()`, and `tab2()` for frequency tables (like `tab`,
  `tab1`, and `tab2` in Stata)
- `fre()`, `fre1()`, and `fre2()` for frequency tables with total rows
  and total columns (like `fre` in Stata)

## Installation

You can install the development version of statart like so:

``` r
devtools::install_github("socimh/statart")
```

## Example

### Load the package

I highly recommend using `tidyverse` with `statart` for better
performance.

``` r
library(statart)
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.2     ✔ readr     2.1.4
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
#> ✔ purrr     1.0.1     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

### `codebook()`

``` r
codebook(starwars)
#> # A tibble: 14 × 4
#>    variable   type          n unique
#>    <chr>      <chr>     <int>  <int>
#>  1 name       character    87     87
#>  2 height     integer      81     45
#>  3 mass       double       59     38
#>  4 hair_color character    82     12
#>  5 skin_color character    87     31
#>  6 eye_color  character    87     15
#>  7 birth_year double       43     36
#>  8 sex        character    83      4
#>  9 gender     character    83      2
#> 10 homeworld  character    77     48
#> 11 species    character    83     37
#> 12 films      list         87     24
#> 13 vehicles   list         87     11
#> 14 starships  list         87     17
codebook(lifeexp)
#> Joining with `by = join_by(variable)`
#> # A tibble: 6 × 5
#>   variable  type          n unique label                   
#>   <chr>     <chr>     <int>  <int> <chr>                   
#> 1 region    labelled     68      3 Region                  
#> 2 country   character    68     68 Country                 
#> 3 popgrowth double       68     30 Avg. annual % growth    
#> 4 lexp      double       68     18 Life expectancy at birth
#> 5 gnppc     double       63     62 GNP per capita          
#> 6 safewater double       40     29 Safe water
```

### `summ()`

``` r
summ(starwars)
#> Warning in check_numeric(.data_summ): 
#>       name, hair_color, skin_color, eye_color, sex, gender, homeworld, species, films, vehicles, starships are non-numeric.
#>       Consider using `tab1()` or `fre1()` instead.
#> # A tibble: 3 × 8
#>   name       type      n unique   min  mean    sd   max
#>   <chr>      <chr> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 height     int      81     45    66 174.   34.8   264
#> 2 mass       dbl      59     38    15  97.3 169.   1358
#> 3 birth_year dbl      43     36     8  87.6 155.    896
summ(lifeexp)
#> Warning in check_numeric(.data_summ): 
#>     country is non-numeric.
#>     Consider using `tab()` or `fre()` instead.
#> Error in `dplyr::summarise()`:
#> ℹ In argument: `dplyr::across(...)`.
#> Caused by error in `across()`:
#> ! Can't compute column `region`.
#> Caused by error in `as.numeric()`:
#> ! Can't convert `x` <haven_labelled> to <double>.
```

### `tab()`

``` r
tab(starwars)
#> # A tibble: 87 × 19
#>    name     height  mass hair_color skin_color eye_color birth_year sex   gender
#>    <chr>     <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#>  1 Ackbar      180  83   none       brown mot… orange          41   male  mascu…
#>  2 Anakin …    188  84   blond      fair       blue            41.9 male  mascu…
#>  3 Ayla Se…    178  55   none       blue       hazel           48   fema… femin…
#>  4 Barriss…    166  50   black      yellow     blue            40   fema… femin…
#>  5 Beru Wh…    165  75   brown      light      blue            47   fema… femin…
#>  6 Biggs D…    183  84   black      light      brown           24   male  mascu…
#>  7 Boba Fe…    183  78.2 black      fair       brown           31.5 male  mascu…
#>  8 Bossk       190 113   none       green      red             53   male  mascu…
#>  9 Chewbac…    228 112   brown      unknown    blue           200   male  mascu…
#> 10 Darth M…    175  80   none       red        yellow          54   male  mascu…
#> # ℹ 77 more rows
#> # ℹ 10 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>, n <int>, percent <dbl>, cum <dbl>,
#> #   valid <dbl>, valid_cum <dbl>
tab(starwars, species)
#> # A tibble: 38 × 6
#>    species       n percent   cum valid valid_cum
#>    <chr>     <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 Aleena        1    1.15  1.15  1.20      1.20
#>  2 Besalisk      1    1.15  2.30  1.20      2.41
#>  3 Cerean        1    1.15  3.45  1.20      3.61
#>  4 Chagrian      1    1.15  4.60  1.20      4.82
#>  5 Clawdite      1    1.15  5.75  1.20      6.02
#>  6 Droid         6    6.90 12.6   7.23     13.3 
#>  7 Dug           1    1.15 13.8   1.20     14.5 
#>  8 Ewok          1    1.15 14.9   1.20     15.7 
#>  9 Geonosian     1    1.15 16.1   1.20     16.9 
#> 10 Gungan        3    3.45 19.5   3.61     20.5 
#> # ℹ 28 more rows
tab(starwars, ends_with("color"))
#> # A tibble: 67 × 8
#>    hair_color    skin_color eye_color     n percent   cum valid valid_cum
#>    <chr>         <chr>      <chr>     <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 auburn        fair       blue          1    1.15  1.15  1.22      1.22
#>  2 auburn, grey  fair       blue          1    1.15  2.30  1.22      2.44
#>  3 auburn, white fair       blue-gray     1    1.15  3.45  1.22      3.66
#>  4 black         blue, grey yellow        1    1.15  4.60  1.22      4.88
#>  5 black         brown      brown         1    1.15  5.75  1.22      6.10
#>  6 black         dark       brown         3    3.45  9.20  3.66      9.76
#>  7 black         dark       dark          1    1.15 10.3   1.22     11.0 
#>  8 black         fair       brown         2    2.30 12.6   2.44     13.4 
#>  9 black         light      brown         1    1.15 13.8   1.22     14.6 
#> 10 black         tan        brown         2    2.30 16.1   2.44     17.1 
#> # ℹ 57 more rows
# using the stata-style syntax in `s_match()`
tab(starwars, s_match("*color")) 
#> # A tibble: 67 × 8
#>    hair_color    skin_color eye_color     n percent   cum valid valid_cum
#>    <chr>         <chr>      <chr>     <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 auburn        fair       blue          1    1.15  1.15  1.22      1.22
#>  2 auburn, grey  fair       blue          1    1.15  2.30  1.22      2.44
#>  3 auburn, white fair       blue-gray     1    1.15  3.45  1.22      3.66
#>  4 black         blue, grey yellow        1    1.15  4.60  1.22      4.88
#>  5 black         brown      brown         1    1.15  5.75  1.22      6.10
#>  6 black         dark       brown         3    3.45  9.20  3.66      9.76
#>  7 black         dark       dark          1    1.15 10.3   1.22     11.0 
#>  8 black         fair       brown         2    2.30 12.6   2.44     13.4 
#>  9 black         light      brown         1    1.15 13.8   1.22     14.6 
#> 10 black         tan        brown         2    2.30 16.1   2.44     17.1 
#> # ℹ 57 more rows

tab1(starwars, s_match("*color"))
#> $hair_color
#> # A tibble: 13 × 6
#>    value             n percent    cum valid valid_cum
#>    <chr>         <int>   <dbl>  <dbl> <dbl>     <dbl>
#>  1 auburn            1    1.15   1.15  1.22      1.22
#>  2 auburn, grey      1    1.15   2.30  1.22      2.44
#>  3 auburn, white     1    1.15   3.45  1.22      3.66
#>  4 black            13   14.9   18.4  15.9      19.5 
#>  5 blond             3    3.45  21.8   3.66     23.2 
#>  6 blonde            1    1.15  23.0   1.22     24.4 
#>  7 brown            18   20.7   43.7  22.0      46.3 
#>  8 brown, grey       1    1.15  44.8   1.22     47.6 
#>  9 grey              1    1.15  46.0   1.22     48.8 
#> 10 none             37   42.5   88.5  45.1      93.9 
#> 11 unknown           1    1.15  89.7   1.22     95.1 
#> 12 white             4    4.60  94.3   4.88    100   
#> 13 <NA>              5    5.75 100    NA        NA   
#> 
#> $skin_color
#> # A tibble: 31 × 6
#>    value                   n percent   cum valid valid_cum
#>    <chr>               <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 blue                    2    2.30  2.30  2.30      2.30
#>  2 blue, grey              2    2.30  4.60  2.30      4.60
#>  3 brown                   4    4.60  9.20  4.60      9.20
#>  4 brown mottle            1    1.15 10.3   1.15     10.3 
#>  5 brown, white            1    1.15 11.5   1.15     11.5 
#>  6 dark                    6    6.90 18.4   6.90     18.4 
#>  7 fair                   17   19.5  37.9  19.5      37.9 
#>  8 fair, green, yellow     1    1.15 39.1   1.15     39.1 
#>  9 gold                    1    1.15 40.2   1.15     40.2 
#> 10 green                   6    6.90 47.1   6.90     47.1 
#> # ℹ 21 more rows
#> 
#> $eye_color
#> # A tibble: 15 × 6
#>    value             n percent   cum valid valid_cum
#>    <chr>         <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 black            10   11.5   11.5 11.5       11.5
#>  2 blue             19   21.8   33.3 21.8       33.3
#>  3 blue-gray         1    1.15  34.5  1.15      34.5
#>  4 brown            21   24.1   58.6 24.1       58.6
#>  5 dark              1    1.15  59.8  1.15      59.8
#>  6 gold              1    1.15  60.9  1.15      60.9
#>  7 green, yellow     1    1.15  62.1  1.15      62.1
#>  8 hazel             3    3.45  65.5  3.45      65.5
#>  9 orange            8    9.20  74.7  9.20      74.7
#> 10 pink              1    1.15  75.9  1.15      75.9
#> 11 red               5    5.75  81.6  5.75      81.6
#> 12 red, blue         1    1.15  82.8  1.15      82.8
#> 13 unknown           3    3.45  86.2  3.45      86.2
#> 14 white             1    1.15  87.4  1.15      87.4
#> 15 yellow           11   12.6  100   12.6      100
tab1(starwars, s_match("*color"), .append = TRUE)
#> # A tibble: 59 × 7
#>    variable   value             n percent   cum valid valid_cum
#>    <chr>      <chr>         <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 hair_color auburn            1    1.15  1.15  1.22      1.22
#>  2 hair_color auburn, grey      1    1.15  2.30  1.22      2.44
#>  3 hair_color auburn, white     1    1.15  3.45  1.22      3.66
#>  4 hair_color black            13   14.9  18.4  15.9      19.5 
#>  5 hair_color blond             3    3.45 21.8   3.66     23.2 
#>  6 hair_color blonde            1    1.15 23.0   1.22     24.4 
#>  7 hair_color brown            18   20.7  43.7  22.0      46.3 
#>  8 hair_color brown, grey       1    1.15 44.8   1.22     47.6 
#>  9 hair_color grey              1    1.15 46.0   1.22     48.8 
#> 10 hair_color none             37   42.5  88.5  45.1      93.9 
#> # ℹ 49 more rows

tab2(starwars, s_match("*i*color"))
#> # A tibble: 13 × 32
#>    `hair_color \\ skin_color`  fair `blue, grey` brown  dark light   tan yellow
#>    <chr>                      <int>        <int> <int> <int> <int> <int>  <int>
#>  1 auburn                         1            0     0     0     0     0      0
#>  2 auburn, grey                   1            0     0     0     0     0      0
#>  3 auburn, white                  1            0     0     0     0     0      0
#>  4 black                          2            1     1     4     1     2      2
#>  5 blond                          3            0     0     0     0     0      0
#>  6 blonde                         0            0     0     0     0     0      0
#>  7 brown                          7            0     2     0     8     0      0
#>  8 brown, grey                    0            0     0     0     1     0      0
#>  9 grey                           0            0     0     0     0     0      0
#> 10 none                           0            1     1     2     1     0      0
#> 11 unknown                        0            0     0     0     0     0      0
#> 12 white                          2            0     0     0     0     0      0
#> 13 <NA>                           0            0     0     0     0     0      0
#> # ℹ 24 more variables: `fair, green, yellow` <int>, unknown <int>, pale <int>,
#> #   blue <int>, `brown mottle` <int>, `brown, white` <int>, green <int>,
#> #   `green, grey` <int>, grey <int>, `grey, blue` <int>,
#> #   `grey, green, yellow` <int>, `grey, red` <int>, metal <int>,
#> #   `mottled green` <int>, none <int>, orange <int>, red <int>,
#> #   `red, blue, white` <int>, `silver, red` <int>, white <int>,
#> #   `white, blue` <int>, gold <int>, `green-tan, brown` <int>, …
```

### `fre()`

`fre()` family functions simply add total rows (and total columns in
`fre2()`) to their corresponding `tab()` functions.

``` r
fre(starwars, species)
#> # A tibble: 41 × 6
#>    species       n percent   cum valid valid_cum
#>    <chr>     <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 Aleena        1    1.15  1.15  1.20      1.20
#>  2 Besalisk      1    1.15  2.30  1.20      2.41
#>  3 Cerean        1    1.15  3.45  1.20      3.61
#>  4 Chagrian      1    1.15  4.60  1.20      4.82
#>  5 Clawdite      1    1.15  5.75  1.20      6.02
#>  6 Droid         6    6.90 12.6   7.23     13.3 
#>  7 Dug           1    1.15 13.8   1.20     14.5 
#>  8 Ewok          1    1.15 14.9   1.20     15.7 
#>  9 Geonosian     1    1.15 16.1   1.20     16.9 
#> 10 Gungan        3    3.45 19.5   3.61     20.5 
#> # ℹ 31 more rows
# `s_print()` usually prints the output in a more readable format
fre(starwars, species) %>% s_print()
#> [31mThere are 41 rows in the dataset.[0m
#> # Top 5 rows:
#> # A tibble: 5 × 6
#>   species      n percent   cum valid valid_cum
#>   <chr>    <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 Aleena       1    1.15  1.15  1.20      1.20
#> 2 Besalisk     1    1.15  2.30  1.20      2.41
#> 3 Cerean       1    1.15  3.45  1.20      3.61
#> 4 Chagrian     1    1.15  4.60  1.20      4.82
#> 5 Clawdite     1    1.15  5.75  1.20      6.02
#> [31m...... 31 rows omitted ......[0m
#> # Bottom 5 rows:
#> # A tibble: 5 × 6
#>   species           n percent   cum  valid valid_cum
#>   <chr>         <int>   <dbl> <dbl>  <dbl>     <dbl>
#> 1 Zabrak            2    2.30  95.4   2.41       100
#> 2 Valid Total      83   95.4   NA   100           NA
#> 3 <NA>              4    4.60 100    NA           NA
#> 4 Missing Total     4    4.60  NA    NA           NA
#> 5 Total           174  100     NA    NA           NA

fre1(starwars, s_match("*color"))
#> $hair_color
#> # A tibble: 16 × 6
#>    value             n percent    cum  valid valid_cum
#>    <chr>         <int>   <dbl>  <dbl>  <dbl>     <dbl>
#>  1 auburn            1    1.15   1.15   1.22      1.22
#>  2 auburn, grey      1    1.15   2.30   1.22      2.44
#>  3 auburn, white     1    1.15   3.45   1.22      3.66
#>  4 black            13   14.9   18.4   15.9      19.5 
#>  5 blond             3    3.45  21.8    3.66     23.2 
#>  6 blonde            1    1.15  23.0    1.22     24.4 
#>  7 brown            18   20.7   43.7   22.0      46.3 
#>  8 brown, grey       1    1.15  44.8    1.22     47.6 
#>  9 grey              1    1.15  46.0    1.22     48.8 
#> 10 none             37   42.5   88.5   45.1      93.9 
#> 11 unknown           1    1.15  89.7    1.22     95.1 
#> 12 white             4    4.60  94.3    4.88    100   
#> 13 Valid Total      82   94.3   NA    100        NA   
#> 14 <NA>              5    5.75 100     NA        NA   
#> 15 Missing Total     5    5.75  NA     NA        NA   
#> 16 Total           174  100     NA     NA        NA   
#> 
#> $skin_color
#> # A tibble: 34 × 6
#>    value                   n percent   cum valid valid_cum
#>    <chr>               <int>   <dbl> <dbl> <dbl>     <dbl>
#>  1 blue                    2    2.30  2.30  2.30      2.30
#>  2 blue, grey              2    2.30  4.60  2.30      4.60
#>  3 brown                   4    4.60  9.20  4.60      9.20
#>  4 brown mottle            1    1.15 10.3   1.15     10.3 
#>  5 brown, white            1    1.15 11.5   1.15     11.5 
#>  6 dark                    6    6.90 18.4   6.90     18.4 
#>  7 fair                   17   19.5  37.9  19.5      37.9 
#>  8 fair, green, yellow     1    1.15 39.1   1.15     39.1 
#>  9 gold                    1    1.15 40.2   1.15     40.2 
#> 10 green                   6    6.90 47.1   6.90     47.1 
#> # ℹ 24 more rows
#> 
#> $eye_color
#> # A tibble: 18 × 6
#>    value             n percent   cum  valid valid_cum
#>    <chr>         <int>   <dbl> <dbl>  <dbl>     <dbl>
#>  1 black            10   11.5   11.5  11.5       11.5
#>  2 blue             19   21.8   33.3  21.8       33.3
#>  3 blue-gray         1    1.15  34.5   1.15      34.5
#>  4 brown            21   24.1   58.6  24.1       58.6
#>  5 dark              1    1.15  59.8   1.15      59.8
#>  6 gold              1    1.15  60.9   1.15      60.9
#>  7 green, yellow     1    1.15  62.1   1.15      62.1
#>  8 hazel             3    3.45  65.5   3.45      65.5
#>  9 orange            8    9.20  74.7   9.20      74.7
#> 10 pink              1    1.15  75.9   1.15      75.9
#> 11 red               5    5.75  81.6   5.75      81.6
#> 12 red, blue         1    1.15  82.8   1.15      82.8
#> 13 unknown           3    3.45  86.2   3.45      86.2
#> 14 white             1    1.15  87.4   1.15      87.4
#> 15 yellow           11   12.6  100    12.6      100  
#> 16 Valid Total      87  100     NA   100         NA  
#> 17 Missing Total     0    0     NA    NA         NA  
#> 18 Total           174  100     NA    NA         NA
fre2(starwars, s_match("*i*color"))
#> # A tibble: 14 × 33
#>    `hair_color \\ skin_color`  fair `blue, grey` brown  dark light   tan yellow
#>    <chr>                      <int>        <int> <int> <int> <int> <int>  <int>
#>  1 auburn                         1            0     0     0     0     0      0
#>  2 auburn, grey                   1            0     0     0     0     0      0
#>  3 auburn, white                  1            0     0     0     0     0      0
#>  4 black                          2            1     1     4     1     2      2
#>  5 blond                          3            0     0     0     0     0      0
#>  6 blonde                         0            0     0     0     0     0      0
#>  7 brown                          7            0     2     0     8     0      0
#>  8 brown, grey                    0            0     0     0     1     0      0
#>  9 grey                           0            0     0     0     0     0      0
#> 10 none                           0            1     1     2     1     0      0
#> 11 unknown                        0            0     0     0     0     0      0
#> 12 white                          2            0     0     0     0     0      0
#> 13 <NA>                           0            0     0     0     0     0      0
#> 14 total                         17            2     4     6    11     2      2
#> # ℹ 25 more variables: `fair, green, yellow` <int>, unknown <int>, pale <int>,
#> #   blue <int>, `brown mottle` <int>, `brown, white` <int>, green <int>,
#> #   `green, grey` <int>, grey <int>, `grey, blue` <int>,
#> #   `grey, green, yellow` <int>, `grey, red` <int>, metal <int>,
#> #   `mottled green` <int>, none <int>, orange <int>, red <int>,
#> #   `red, blue, white` <int>, `silver, red` <int>, white <int>,
#> #   `white, blue` <int>, gold <int>, `green-tan, brown` <int>, …
```

## Code of Conduct

Please note that the statart project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
