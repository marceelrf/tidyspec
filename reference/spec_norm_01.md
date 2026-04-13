# Normalize Spectral Data to the \[0, 1\] Range

This function normalizes the numeric spectral data in each column to the
\[0, 1\] range, preserving the wavelength column.

## Usage

``` r
spec_norm_01(.data, wn_col = NULL)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

## Value

A \`tibble\` with the normalized spectral data, containing the
wavelength column and the normalized numeric columns.
