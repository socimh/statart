
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Packages

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/Packages)](https://CRAN.R-project.org/package=Packages)

<!-- badges: end -->

The primary goal of statart is to migrate some useful commands
(functions) from Stata to R. Functions are built in the tidyverse
framework, including

- `tab()` and `tab1()` for frequency tables (like `tab` and `tab1` in
  Stata)
- `fre()` for frequency tables with missing information (like `fre` in
  Stata)
- `summ()` for summary statistics (like `summarize` in Stata)
- `codebook()` for codebook (like `codebook` in Stata)

## Installation

You can install the development version of Packages like so:

``` r
devtools::install_github("socimh/statart")
```

## Example

This is a basic example which shows you how to solve a common problem: