# Apply Differentiation to Spectral Data

This function applies numerical differentiation to spectral data,
allowing for the calculation of the first or higher-order differences.

## Usage

``` r
spec_diff(.data, wn_col = NULL, degree = 1)
```

## Arguments

- .data:

  A \`data.frame\` or \`tibble\` containing spectral data.

- wn_col:

  A character string specifying the column name for the wavelength data.
  Default is \`"Wn"\`.

- degree:

  A numeric value specifying the degree of differentiation. If
  \`degree\` is 0, the original data is returned without any changes.

## Value

A \`tibble\` with the differentiated spectral data, containing the
wavelength column and the differentiated numeric columns. If \`degree\`
is 0, the original data is returned.
