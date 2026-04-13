# Interpolate Spectral Data to Include Both Grids (Right Join Style)

This function interpolates reference data to match the wavenumber grid
of the main dataset. Works like a right join - keeps all wavenumbers
from .data and interpolates reference data to match those wavenumbers.

## Usage

``` r
spec_interpolate_right(
  .data,
  reference,
  wn_col = NULL,
  method = "pchip",
  extrapolate = FALSE,
  suffix = c(".x", ".y"),
  ...
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing the main spectral data.

- reference:

  A \`data.frame\` or \`tibble\` containing spectral data to be
  interpolated.

- wn_col:

  A character string specifying the column name for the wavenumber data.

- method:

  A character string specifying the interpolation method.

- extrapolate:

  Logical. Should values be extrapolated outside the original range?

- suffix:

  Character vector of length 2 specifying suffixes for overlapping
  column names.

- ...:

  Additional arguments passed to the interpolation method.

## Value

A \`tibble\` combining .data with interpolated reference data.
