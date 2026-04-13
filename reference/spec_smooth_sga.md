# Apply Savitzky-Golay Smoothing to Spectral Data

This function applies Savitzky-Golay smoothing to numeric spectral data
using a specified window size, polynomial order, and differentiation
degree, while preserving the wavelength column.

## Usage

``` r
spec_smooth_sga(.data, wn_col = NULL, window = 15, forder = 4, degree = 0)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

- window:

  A numeric value specifying the window size for the Savitzky-Golay
  smoothing. Default is 15.

- forder:

  A numeric value specifying the polynomial order for smoothing. Default
  is 4.

- degree:

  A numeric value specifying the degree of differentiation. Default is 0
  (no differentiation).

## Value

A \`tibble\` with the smoothed spectral data, containing the wavelength
column and the smoothed numeric columns.
