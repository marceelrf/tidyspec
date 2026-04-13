# CoHAspec Dataset

A dataset containing spectral absorbance measurements for different
concentrations of CoHA.

## Usage

``` r
CoHAspec
```

## Format

A data frame with 6 rows and 5 columns:

- Wavenumber:

  Numeric. The spectral wavenumber (cm-1).

- CoHA01:

  Numeric. Absorbance values for CoHA at 1 mM Cobalt concentration.

- CoHA025:

  Numeric. Absorbance values for CoHA at 2.5 mM Cobalt concentration.

- CoHA05:

  Numeric. Absorbance values for CoHA at 5 mM Cobalt concentration.

- CoHA100:

  Numeric. Absorbance values for CoHA at 10 mM Cobalt concentration.

## Source

de Almeida GS, Ferreira MR, da Costa Fernandes CJ, et al. Development of
cobalt (Co)-doped monetites for bone regeneration. J Biomed Mater Res.
2024; 112(1):e35319. \<doi:10.1002/jbm.b.35319\>

## Examples

``` r
data(CoHAspec)
head(CoHAspec)
#> # A tibble: 6 × 5
#>   Wavenumber CoHA01 CoHA025 CoHA05 CoHA100
#>        <dbl>  <dbl>   <dbl>  <dbl>   <dbl>
#> 1       399.  0.871    1.36  1.17    1.05 
#> 2       401.  0.893    1.24  1.05    0.925
#> 3       403.  0.910    1.20  0.997   0.876
#> 4       405.  0.914    1.19  0.982   0.867
#> 5       407.  0.908    1.18  0.965   0.857
#> 6       409.  0.887    1.14  0.936   0.828
```
