# Standardize Spectral Data to Unit Variance

This function standardizes the numeric spectral data in each column to
have a mean of 0 and a standard deviation of 1 (unit variance), while
preserving the wavelength column.

## Usage

``` r
spec_norm_var(.data, wn_col = NULL)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

## Value

A \`tibble\` with the standardized spectral data, containing the
wavelength column and the standardized numeric columns.
