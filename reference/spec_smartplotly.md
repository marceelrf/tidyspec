# Create an Interactive Plot for Spectral Data (plotly)

This function generates a customizable interactive plot for spectral
data using plotly, allowing for the selection of plot type (absorbance
or transmittance), x-axis direction, plot geometry (points or lines),
and color palette.

## Usage

``` r
spec_smartplotly(
  .data,
  wn_col = NULL,
  xdir = c("reverse", "standard"),
  geom = c("point", "line"),
  xmin = NULL,
  xmax = NULL,
  alpha = 0.8,
  type = c("absorbance", "transmittance"),
  palette = c("viridis", "plasma", "magma", "cividis", "turbo", "Set1", "Set2", "Dark2",
    "Paired", "custom"),
  custom_colors = NULL
)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength or
  wavenumber data. This parameter is required.

- xdir:

  A character string specifying the direction of the x-axis. Choices are
  \`"reverse"\` (typically used for wavenumber) or \`"standard"\`.

- geom:

  A character string specifying the geometry of the plot. Choices are
  \`"point"\` for a scatter plot or \`"line"\` for a line plot.

- xmin:

  A numeric value specifying the minimum x-axis value. If not provided,
  the minimum value from \`wn_col\` will be used.

- xmax:

  A numeric value specifying the maximum x-axis value. If not provided,
  the maximum value from \`wn_col\` will be used.

- alpha:

  A numeric value (0–1) specifying the transparency of plotted points or
  lines. Default is \`0.8\`.

- type:

  A character string specifying the y-axis label. Either
  \`"absorbance"\` (default) or \`"transmittance"\`.

- palette:

  A character string specifying the color palette to use. Built-in
  options:

  \`"viridis"\`

  :   Perceptually uniform, colorblind-friendly (default).

  \`"plasma"\`

  :   High-contrast warm-to-cool gradient.

  \`"magma"\`

  :   Dark-to-light, suitable for dark backgrounds.

  \`"cividis"\`

  :   Optimized for color vision deficiency.

  \`"turbo"\`

  :   Full-spectrum rainbow with high contrast.

  \`"Set1"\`

  :   Qualitative palette from RColorBrewer (up to 9 colors).

  \`"Set2"\`

  :   Softer qualitative palette (up to 8 colors).

  \`"Dark2"\`

  :   Darker qualitative palette (up to 8 colors).

  \`"Paired"\`

  :   Paired qualitative palette (up to 12 colors).

  \`"custom"\`

  :   Use a custom vector of colors via \`custom_colors\`.

- custom_colors:

  A character vector of valid color strings (hex codes or R color names)
  used when \`palette = "custom"\`. The number of colors should match or
  exceed the number of spectra in the data. If fewer colors are provided
  than spectra, colors will be recycled with a warning.

## Value

A \`plotly\` object representing the interactive spectral plot.

## Examples

``` r
if (FALSE) { # \dontrun{
# Default viridis palette, line geometry
spec_smartplotly(spec_data, wn_col = "wavenumber", geom = "line")

# Built-in qualitative palette
spec_smartplotly(spec_data, wn_col = "wavenumber", palette = "Set1")

# Custom colors
spec_smartplotly(spec_data, wn_col = "wavenumber",
                      palette = "custom",
                      custom_colors = c("#E63946", "#457B9D", "#2A9D8F"))
} # }
```
