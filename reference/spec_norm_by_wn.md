# Normalize Spectral Data by Specific Wavenumber Value

This function normalizes the numeric spectral data in each column so
that the intensity at a specified wavenumber equals a target value
(default is 1), while preserving the wavelength column.

## Usage

``` r
spec_norm_by_wn(
  .data,
  wn_col = NULL,
  target_wn,
  target_value = 1,
  method = "linear"
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  If NULL, uses the default set by \`set_spec_wn()\`.

- target_wn:

  A numeric value specifying the wavenumber to use as reference for
  normalization.

- target_value:

  A numeric value specifying the target intensity value at the reference
  wavenumber. Default is 1.

- method:

  A character string specifying the interpolation method when the exact
  wavenumber is not found. Options are "linear" (default) or "nearest".

## Value

A \`tibble\` with the wavenumber-normalized spectral data, containing
the wavelength column and the normalized numeric columns.
