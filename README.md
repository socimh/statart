
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
```

### codebook()

View the codebook of `lifeexp`.

``` r
codebook(lifeexp)
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

### summ()

Summarise the numeric variables in `lifeexp`.

``` r
summ(lifeexp)
#> Warning in check_numeric(.data_summ): 
#>     country is non-numeric.
#>     Consider using `tab()` or `fre()` instead.
#> # A tibble: 5 × 8
#>   name      type      n unique   min     mean        sd   max
#>   <chr>     <chr> <dbl>  <dbl> <dbl>    <dbl>     <dbl> <dbl>
#> 1 region*** lbl      68      3   1      1.5       0.743     3
#> 2 popgrowth dbl      68     30  -0.5    0.972     0.931     3
#> 3 lexp      dbl      68     18  54     72.3       4.72     79
#> 4 gnppc     dbl      63     62 370   8675.    10635.    39980
#> 5 safewater dbl      40     29  28     76.1      17.9     100
```

Not that `region` is a factor variable, so the mean and standard
deviation of it may be meaningless.

### tab()

Tabulate a single variable:

``` r
tab(starwars, sex)
#> # A tibble: 5 × 6
#>   sex                n percent   cum valid valid_cum
#>   <chr>          <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 female            16   18.4   18.4 19.3       19.3
#> 2 hermaphroditic     1    1.15  19.5  1.20      20.5
#> 3 male              60   69.0   88.5 72.3       92.8
#> 4 none               6    6.90  95.4  7.23     100  
#> 5 <NA>               4    4.60 100   NA         NA
```

`s_match()` can select variables in a stata style.

``` r
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
```

`tab1()` tabulates variables one by one as a list.

``` r
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
```

`.append` can flatten the list into a tibble.

``` r
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
```

`tab2()` cross-tabulates two variables.

``` r
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

### fre()

`fre()` family functions simply add total rows (and total columns in
`fre2()`) to their `tab()` counterparts.

``` r
fre(starwars, sex)
#> # A tibble: 8 × 6
#>   sex                n percent   cum  valid valid_cum
#>   <chr>          <int>   <dbl> <dbl>  <dbl>     <dbl>
#> 1 female            16   18.4   18.4  19.3       19.3
#> 2 hermaphroditic     1    1.15  19.5   1.20      20.5
#> 3 male              60   69.0   88.5  72.3       92.8
#> 4 none               6    6.90  95.4   7.23     100  
#> 5 Valid Total       83   95.4   NA   100         NA  
#> 6 <NA>               4    4.60 100    NA         NA  
#> 7 Missing Total      4    4.60  NA    NA         NA  
#> 8 Total            174  100     NA    NA         NA
```

As the “Total” row is added, subsequent analysis on the result becomes
more complicated, and the categories of the variable will be always
converted to `character` (i.e., the same type as “Total”). However, I
still keep these functions as they print the results with more
comprehensive information.

### s\_…() tools

“s” stands for `statart` as “st\_” has already be taken by the powerful
geospatial package `sf`. Let me briefly introduce some of them here.

`s_select()` extends the `select()` in tidyverse to enable data-masking
features. For example,

``` r
starwars %>%
  s_select(tall_characters = height > 100)
#> # A tibble: 87 × 1
#>    tall_characters
#>    <lgl>          
#>  1 TRUE           
#>  2 TRUE           
#>  3 FALSE          
#>  4 TRUE           
#>  5 TRUE           
#>  6 TRUE           
#>  7 TRUE           
#>  8 FALSE          
#>  9 TRUE           
#> 10 TRUE           
#> # ℹ 77 more rows
```

If you do not know whether a function works without any error, you could
use `s_try()`.

``` r
starwars %>%
  select(tall_characters = height > 100) %>%
  s_try()
#> [1] FALSE

starwars %>%
  s_select(tall_characters = height > 100) %>%
  s_try()
#> [1] TRUE
```

Built on `pillar::type_sum()` and `vctrs::vec_ptype_abbr()`, `s_type()`
returns the type of input object.

``` r
s_type(lifeexp)
#> [1] "tibble"

s_type(lifeexp$region, .full = TRUE)
#> [1] "labelled"
```

`s_print()` is designed for viewing long data by their top and bottom
rows.

``` r
s_print(lifeexp)
#> [31mThere are 68 rows in the dataset.[0m
#> # Top 5 rows:
#> # A tibble: 5 × 6
#>   region               country    popgrowth  lexp gnppc safewater
#>   <dbl+lbl>            <chr>          <dbl> <dbl> <dbl>     <dbl>
#> 1 1 [Europe & C. Asia] Albania        1.20     72   810        76
#> 2 1 [Europe & C. Asia] Armenia        1.10     74   460        NA
#> 3 1 [Europe & C. Asia] Austria        0.400    79 26830        NA
#> 4 1 [Europe & C. Asia] Azerbaijan     1.40     71   480        NA
#> 5 1 [Europe & C. Asia] Belarus        0.300    68  2180        NA
#> [31m...... 58 rows omitted ......[0m
#> # Bottom 5 rows:
#> # A tibble: 5 × 6
#>   region            country   popgrowth  lexp gnppc safewater
#>   <dbl+lbl>         <chr>         <dbl> <dbl> <dbl>     <dbl>
#> 1 3 [South America] Ecuador       2.40     70  1520        70
#> 2 3 [South America] Paraguay      2.90     70  1760        39
#> 3 3 [South America] Peru          2        69  2440        80
#> 4 3 [South America] Uruguay       0.700    74  6070        89
#> 5 3 [South America] Venezuela     2.40     73  3530        79

lifeexp %>%
  fre(safewater) %>%
  s_print()
#> [31mThere are 33 rows in the dataset.[0m
#> # Top 5 rows:
#> # A tibble: 5 × 6
#>   safewater     n percent   cum valid valid_cum
#>   <chr>     <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 28            1    1.47  1.47   2.5       2.5
#> 2 39            1    1.47  2.94   2.5       5  
#> 3 55            3    4.41  7.35   7.5      12.5
#> 4 56            1    1.47  8.82   2.5      15  
#> 5 57            1    1.47 10.3    2.5      17.5
#> [31m...... 23 rows omitted ......[0m
#> # Bottom 5 rows:
#> # A tibble: 5 × 6
#>   safewater         n percent   cum valid valid_cum
#>   <chr>         <int>   <dbl> <dbl> <dbl>     <dbl>
#> 1 100               5    7.35  58.8  12.5       100
#> 2 Valid Total      40   58.8   NA   100          NA
#> 3 <NA>             28   41.2  100    NA          NA
#> 4 Missing Total    28   41.2   NA    NA          NA
#> 5 Total           136  100     NA    NA          NA
```

`s_time()` shows the time spent on running a function:

``` r
summ_result <- lifeexp %>%
  summ() %>%
  s_time()
#> Warning in check_numeric(.data_summ): 
#>     country is non-numeric.
#>     Consider using `tab()` or `fre()` instead.
#> Time spent: 0.052 secs
```

… and it does not affect the function result.

``` r
summ_result
#> # A tibble: 5 × 8
#>   name      type      n unique   min     mean        sd   max
#>   <chr>     <chr> <dbl>  <dbl> <dbl>    <dbl>     <dbl> <dbl>
#> 1 region*** lbl      68      3   1      1.5       0.743     3
#> 2 popgrowth dbl      68     30  -0.5    0.972     0.931     3
#> 3 lexp      dbl      68     18  54     72.3       4.72     79
#> 4 gnppc     dbl      63     62 370   8675.    10635.    39980
#> 5 safewater dbl      40     29  28     76.1      17.9     100
```

## Code of Conduct

Please note that the statart project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
