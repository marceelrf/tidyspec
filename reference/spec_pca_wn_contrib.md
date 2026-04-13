# Compute Wavenumber Contributions to Principal Components

This function calculates the contribution of each wavenumber to the
principal components (PCs) in a PCA result. Contributions are computed
as the squared loadings multiplied by 100.

This function calculates the contribution of each wavenumber to the
principal components (PCs) in a PCA result. Contributions are computed
as the squared loadings multiplied by 100.

## Usage

``` r
spec_pca_wn_contrib(PCA)

spec_pca_wn_contrib(PCA)
```

## Arguments

- PCA:

  An object of class \`prcomp\`, containing the results of a principal
  component analysis.

## Value

A tibble containing the wavenumber column and the percentage
contribution of each wavenumber to each principal component.

A tibble containing the wavenumber column and the percentage
contribution of each wavenumber to each principal component.

## Details

The function extracts the PCA loadings (rotation matrix) and computes
the squared values of each loading, scaled to percentage values. This
helps interpret the importance of each wavenumber in defining the
principal components.

The function extracts the PCA loadings (rotation matrix) and computes
the squared values of each loading, scaled to percentage values. This
helps interpret the importance of each wavenumber in defining the
principal components.

## Examples

``` r
# \donttest{
pca_result <- spec_pca(CoHAspec)
wn_contrib <- spec_pca_wn_contrib(pca_result)
print(wn_contrib)
#> # A tibble: 1,868 × 5
#>    Wavenumber      PC1   PC2   PC3      PC4
#>         <dbl>    <dbl> <dbl> <dbl>    <dbl>
#>  1       399. 0.00122  0.118 0.855 50.7    
#>  2       401. 0.000366 0.184 0.558  0.00698
#>  3       403. 0.00328  0.212 0.362  8.13   
#>  4       405. 0.00485  0.211 0.331  0.00642
#>  5       407. 0.00586  0.210 0.317  0.00453
#>  6       409. 0.00619  0.216 0.280  0.0147 
#>  7       411. 0.00597  0.226 0.237  0.00187
#>  8       413. 0.00524  0.237 0.201  0.0152 
#>  9       415. 0.00442  0.245 0.178  0.0509 
#> 10       417. 0.00120  0.232 0.309  0.0154 
#> # ℹ 1,858 more rows
# }

# \donttest{
pca_result <- spec_pca(CoHAspec)
wn_contrib <- spec_pca_wn_contrib(pca_result)
print(wn_contrib)
#> # A tibble: 1,868 × 5
#>    Wavenumber      PC1   PC2   PC3      PC4
#>         <dbl>    <dbl> <dbl> <dbl>    <dbl>
#>  1       399. 0.00122  0.118 0.855 50.7    
#>  2       401. 0.000366 0.184 0.558  0.00698
#>  3       403. 0.00328  0.212 0.362  8.13   
#>  4       405. 0.00485  0.211 0.331  0.00642
#>  5       407. 0.00586  0.210 0.317  0.00453
#>  6       409. 0.00619  0.216 0.280  0.0147 
#>  7       411. 0.00597  0.226 0.237  0.00187
#>  8       413. 0.00524  0.237 0.201  0.0152 
#>  9       415. 0.00442  0.245 0.178  0.0509 
#> 10       417. 0.00120  0.232 0.309  0.0154 
#> # ℹ 1,858 more rows
# }
```
