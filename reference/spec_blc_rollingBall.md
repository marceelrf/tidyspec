# Apply Rolling Ball Baseline Correction to Spectral Data

This function applies a rolling ball baseline correction to spectral
data within a specified wavelength range. It allows for correction of
either absorbance or transmittance data.

## Usage

``` r
spec_blc_rollingBall(
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
  converted to absorbance before applying the baseline correction.

## Value

A \`tibble\` with the baseline-corrected spectral data, containing the
wavelength column and the corrected numeric columns.

## References

Baseline estimation performed using a custom rolling ball
implementation.
