
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

### `codebook()`

``` r
codebook(starwars)
codebook(lifeexp)
```

### `summ()`

``` r
summ(starwars)
summ(lifeexp)
```

### `tab()`

``` r
tab(starwars)
tab(starwars, species)
tab(starwars, ends_with("color"))
# using the stata-style syntax in `s_match()`
tab(starwars, s_match("*color")) 

tab1(starwars, s_match("*color"))
tab1(starwars, s_match("*color"), .append = TRUE)

tab2(starwars, s_match("*i*color"))
```

### `fre()`

`fre()` family functions simply add total rows (and total columns in
`fre2()`) to their corresponding `tab()` functions.

``` r
fre(starwars, species)
# `s_print()` usually prints the output in a more readable format
fre(starwars, species) %>% s_print()

fre1(starwars, s_match("*color"))
fre2(starwars, s_match("*i*color"))
```

## Code of Conduct

Please note that the statart project is released with a [Contributor
Code of
Conduct](https://www.contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
