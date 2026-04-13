# Perform Principal Component Analysis (PCA) on Spectral Data

This function computes a Principal Component Analysis (PCA) on spectral
data, excluding the wavenumber column from the analysis.

## Usage

``` r
spec_pca(.data, wn_col = NULL, scale = TRUE, center = TRUE)
```

## Arguments

- .data:

  A data frame containing spectral data, with one column representing
  wavenumbers and the remaining columns containing spectral intensity
  values.

- wn_col:

  A string specifying the name of the column that contains the
  wavenumber values. If NULL, the function attempts to retrieve the
  default wavenumber column set by \`set_spec_wn()\`.

- scale:

  A logical value indicating whether the spectral data should be scaled
  (default is TRUE).

- center:

  A logical value indicating whether the spectral data should be
  centered (default is TRUE).

## Value

A \`prcomp\` object containing the PCA results, including principal
components, standard deviations, and loadings.

## Examples

``` r
# \donttest{
set_spec_wn("Wavenumber")
#> Successfully set 'Wavenumber' as the default wavenumber column.
pca_result <- spec_pca(CoHAspec)
summary(pca_result)
#> Importance of components:
#>                            PC1     PC2     PC3       PC4
#> Standard deviation     38.3080 18.1977 8.32702 3.398e-14
#> Proportion of Variance  0.7856  0.1773 0.03712 0.000e+00
#> Cumulative Proportion   0.7856  0.9629 1.00000 1.000e+00
# }
```
