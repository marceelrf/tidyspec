
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyspec <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/marceelrf/tidyspec/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/marceelrf/tidyspec/actions/workflows/R-CMD-check.yaml)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of **tidyspec** is to provide a friendly pipeline for
spectroscopy analysis using the tidy data philosophy.

## Installation

You can install the development version of tidyspec from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("marceelrf/tidyspec")
```

## About

The `tidyspec` package was design to enable the data analysis of
spectroscopy data (as IR, Raman, NMR) with the tidy-data format. There
are 6 families of functions in `tidyspec`, all starting with `spec_`:

- **Transformation**: Convert data from absorbance to transmittance
  (`spec_abs2trans`) & from transmittance to absorbance
  (`spec_trans2abs`).  

- **Normalize**: Normalize the data to range 0-1 (`spec_norm_01`),
  normalize between a custom range (`spec_norm_minmax`), or normalize to
  have a standard deviation of one (`spec_norm_var`).  

- **Baseline correction**: Correct the baseline using the the *rolling
  ball* algorithm (`spec_blc_rollingBall`) or *Iterative Restricted
  Least Squares* (`spec_blc_irls`). The function `spec_bl` return the
  baseline vectors (`spec_bl_rollingBall`, `spec_bl_irls`).

- **Smooth correction**: Smooth the data using the average window
  (`spec_smooth_avg`) or using the Savitzky-Golay algorithm
  (`spec_smooth_sga`).  

- **Derivative**: Create differential data from the spectra
  (`spec_diff`).  

- **Preview**: Preview your data while applying changes statically
  (`spec_smartplot`) or interactively (`spec_smartplotly`).

- **Import/Export**: Import spectra data from common data formats, like
  csv, txt, tsv, xslx and xls, with `spec_read`. Export functions will
  be created in next moment, but user can easily use `{readr}` or
  `{writexl}` functions.

The function `set_spec_wn` simplifies the use of functions by globally
defining the column that contains the wave numbers. User can check the
wavenumber column with `check_wn_col`.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidyspec)
## basic example code
```
