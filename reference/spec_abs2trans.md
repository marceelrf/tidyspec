# Convert Absorbance Data to Transmittance

This function converts absorbance data to transmittance using the
formula \\T = 10^{(2 - A)}\\, where \\A\\ is the absorbance and \\T\\ is
the transmittance.

## Usage

``` r
spec_abs2trans(.data, wn_col = NULL)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data in absorbance.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

## Value

A \`tibble\` with the converted transmittance data, containing the
wavelength column and the numeric transmittance columns. Any rows with
infinite values are removed.
