# Convert Spectral Data from Transmittance to Absorbance

This function converts transmittance data to absorbance using the
formula \`A = 2 - log10(T)\`, where \`T\` is the transmittance. It also
filters out any infinite values resulting from the transformation, while
preserving the wavelength column.

## Usage

``` r
spec_trans2abs(.data, wn_col = NULL)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

## Value

A \`tibble\` with the converted absorbance data, containing the
wavelength column and the absorbance numeric columns.
