# Extract Rolling Ball Baseline from Spectral Data

This function extracts the rolling ball baseline from spectral data
within a specified wavelength range. It returns only the baseline, not
the corrected data.

## Usage

``` r
spec_bl_rollingBall(
  .data,
  wn_col = NULL,
  wn_min = NULL,
  wn_max = NULL,
  wm,
  ws = 0,
  is_abs = TRUE
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

- wn_min:

  A numeric value specifying the minimum wavelength to consider for the
  baseline correction.

- wn_max:

  A numeric value specifying the maximum wavelength to consider for the
  baseline correction.

- wm:

  A numeric value for the window size of the rolling ball algorithm.

- ws:

  A numeric value for the smoothing factor of the rolling ball
  algorithm.

- is_abs:

  A logical value indicating whether the data is already in absorbance.
  If \`TRUE\`, absorbance is used directly; if \`FALSE\`, the data is
  converted to absorbance before extracting the baseline.

## Value

A \`tibble\` with the baseline data, containing the wavelength column
and the baseline for each numeric column.

## References

Baseline estimation performed using a custom rolling ball
implementation.
