# Filter spectral data by wavenumber range

This function filters the spectral dataset based on a specified
wavenumber (\`wn\`) range. It requires the wavenumber column to be
previously set using \[set_spec_wn()\]. If \`wn_min\` and/or \`wn_max\`
are provided, the data will be filtered accordingly. If neither is
provided, the original dataset is returned unchanged.

## Usage

``` r
spec_filter(.data, wn_min = NULL, wn_max = NULL)
```

## Arguments

- .data:

  A data frame containing spectral data.

- wn_min:

  Optional numeric value. Minimum wavenumber value to keep.

- wn_max:

  Optional numeric value. Maximum wavenumber value to keep.

## Value

A filtered data frame based on the wavenumber column.

## Examples

``` r
set_spec_wn("Wavenumber")
#> Successfully set 'Wavenumber' as the default wavenumber column.
spec_filter(CoHAspec, wn_min = 500, wn_max = 1800)
#> # A tibble: 674 × 5
#>    Wavenumber CoHA01 CoHA025 CoHA05 CoHA100
#>         <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
#>  1       501.  0.894   0.951  0.802   0.649
#>  2       503.  0.908   0.962  0.811   0.658
#>  3       505.  0.921   0.972  0.818   0.666
#>  4       507.  0.935   0.985  0.829   0.678
#>  5       509.  0.951   0.999  0.838   0.689
#>  6       511.  0.969   1.02   0.849   0.702
#>  7       513.  0.987   1.03   0.862   0.718
#>  8       515.  1.00    1.05   0.878   0.737
#>  9       517.  1.02    1.08   0.897   0.762
#> 10       519.  1.05    1.10   0.914   0.787
#> # ℹ 664 more rows
```
