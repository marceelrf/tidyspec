# Apply Smoothing to Spectral Data Using a Moving Average

This function applies a moving average smoothing to numeric spectral
data using a specified window size and polynomial degree, while
preserving the wavelength column.

## Usage

``` r
spec_smooth_avg(.data, wn_col = NULL, window = 15, degree = 2)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

- window:

  A numeric value specifying the window size for the moving average
  smoothing. Default is 15.

- degree:

  A numeric value specifying the degree of the polynomial for smoothing.
  Default is 2.

## Value

A \`tibble\` with the smoothed spectral data, containing the wavelength
column and the smoothed numeric columns.
