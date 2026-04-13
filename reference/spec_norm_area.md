# Normalize Spectral Data by Area Under the Curve

This function normalizes the numeric spectral data in each column so
that the area under the curve equals a specified value (default is 1),
while preserving the wavelength column.

## Usage

``` r
spec_norm_area(.data, wn_col = NULL, target_area = 1)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  If NULL, uses the default set by \`set_spec_wn()\`.

- target_area:

  A numeric value specifying the target area under the curve. Default is
  1.

## Value

A \`tibble\` with the area-normalized spectral data, containing the
wavelength column and the normalized numeric columns.
