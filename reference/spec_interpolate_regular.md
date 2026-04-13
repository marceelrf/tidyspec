# Interpolate to Regular Grid

Creates a regular wavenumber grid and interpolates spectral data to
match it.

## Usage

``` r
spec_interpolate_regular(
  .data,
  resolution = 1,
  wn_min = NULL,
  wn_max = NULL,
  wn_col = NULL,
  method = "pchip",
  ...
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- resolution:

  Numeric. The step size for the regular grid.

- wn_min, wn_max:

  Numeric. Range for the regular grid. If NULL, uses data range.

- wn_col:

  A character string specifying the column name for the wavenumber data.

- method:

  A character string specifying the interpolation method.

- ...:

  Additional arguments passed to the interpolation method.

## Value

A \`tibble\` with spectral data interpolated to a regular grid.
