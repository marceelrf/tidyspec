# Normalize Spectral Data to a Specified Range

This function normalizes the numeric spectral data in each column to a
specified range \[min, max\], preserving the wavelength column.

## Usage

``` r
spec_norm_minmax(.data, wn_col = NULL, min = 0, max = 1)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

- min:

  A numeric value specifying the minimum value of the desired range.
  Default is 0.

- max:

  A numeric value specifying the maximum value of the desired range.
  Default is 1.

## Value

A \`tibble\` with the normalized spectral data, containing the
wavelength column and the normalized numeric columns.
