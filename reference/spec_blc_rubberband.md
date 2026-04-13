# Apply Rubberband Baseline Correction to Spectral Data

This function applies a rubberband baseline correction to spectral data
within a specified wavelength range. The rubberband method fits a
baseline by connecting local minima points in the spectrum. It allows
for correction of either absorbance or transmittance data.

## Usage

``` r
spec_blc_rubberband(
  .data,
  wn_col = NULL,
  wn_min = NULL,
  wn_max = NULL,
  segment_length = 50,
  smooth_baseline = TRUE,
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

- segment_length:

  A numeric value specifying the length of segments for finding local
  minima. Default is 50.

- smooth_baseline:

  A logical value indicating whether to smooth the baseline using spline
  interpolation. Default is TRUE.

- is_abs:

  A logical value indicating whether the data is already in absorbance.
  If \`TRUE\`, absorbance is used directly; if \`FALSE\`, the data is
  converted to absorbance before applying the baseline correction.

## Value

A \`tibble\` with the baseline-corrected spectral data, containing the
wavelength column and the corrected numeric columns.

## References

Rubberband baseline correction connects local minima to estimate
baseline.
