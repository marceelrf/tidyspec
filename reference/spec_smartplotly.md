# Create an Interactive Plot for Spectral Data using Plotly

This function generates an interactive Plotly plot for spectral data,
allowing for the selection of plot type (absorbance or transmittance),
x-axis direction, and plot geometry (points or lines).

## Usage

``` r
spec_smartplotly(
  .data,
  wn_col = NULL,
  type = c("absorbance", "transmittance"),
  xdir = c("reverse", "standard"),
  geom = c("point", "line"),
  xmin = NULL,
  xmax = NULL,
  alpha = 0.8
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength or
  wavenumber data. Default is \`"Wn"\`.

- type:

  A character string specifying the type of data to plot. Choices are
  \`"absorbance"\` or \`"transmittance"\`.

- xdir:

  A character string specifying the direction of the x-axis. Choices are
  \`"reverse"\` for reverse direction (typically used for wavenumber) or
  \`"standard"\` for standard direction.

- geom:

  A character string specifying the geometry of the plot. Choices are
  \`"point"\` for a scatter plot or \`"line"\` for a line plot.

- xmin:

  A numeric value specifying the minimum x-axis value for the plot. If
  not provided, the minimum value from the \`wn_col\` data will be used.

- xmax:

  A numeric value specifying the maximum x-axis value for the plot. If
  not provided, the maximum value from the \`wn_col\` data will be used.

- alpha:

  A numeric value specifying the transparency level of the plotted
  points or lines. Default is 0.8.

## Value

A \`plotly\` object representing the interactive spectral plot.
