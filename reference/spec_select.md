# Select Specific Columns in a Spectral Data Frame

This function selects user-specified columns from a spectral dataset,
always ensuring that the wavenumber column (\`wn_col\`) is included,
unless explicitly excluded.

## Usage

``` r
spec_select(.data, ...)
```

## Arguments

- .data:

  A data frame containing spectral data.

- ...:

  Column selection helpers (e.g., column names, -column_to_exclude).

## Value

A data frame containing the selected columns.

## See also

\[dplyr::select()\], \[set_spec_wn()\]
