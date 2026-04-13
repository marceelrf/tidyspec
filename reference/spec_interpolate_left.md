# Interpolate Spectral Data to Match Reference Wavenumbers (Left Join Style)

This function interpolates spectral data to match the wavenumber grid of
a reference dataset. Works like a left join - keeps all wavenumbers from
the reference data and interpolates the spectral data to match those
wavenumbers.

## Usage

``` r
spec_interpolate_left(
  .data,
  reference,
  wn_col = NULL,
  method = "pchip",
  extrapolate = FALSE,
  ...
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data to be
  interpolated.

- reference:

  A \`data.frame\` or \`tibble\` containing the reference wavenumber
  grid.

- wn_col:

  A character string specifying the column name for the wavenumber data.
  If NULL, uses the default set with set_spec_wn().

- method:

  A character string specifying the interpolation method. Options:
  "linear", "pchip", "spline", "akima". Default is "pchip".

- extrapolate:

  Logical. Should values be extrapolated outside the original range?
  Default is FALSE (returns NA for out-of-range values).

- ...:

  Additional arguments passed to the interpolation method.

## Value

A \`tibble\` with interpolated spectral data using the reference
wavenumber grid.
