# Enhanced Rolling Ball with Mathematical Morphology

More sophisticated version of rolling ball using mathematical morphology
concepts

## Usage

``` r
rolling_ball_morphology(x, radius, smooth = TRUE)
```

## Arguments

- x:

  Numeric vector containing the spectrum/signal values

- radius:

  Radius of the structuring ball

- smooth:

  Apply additional smoothing (logical)

## Value

A list with three components:

- baseline:

  Numeric vector containing the estimated baseline values using
  morphological operations

- corrected:

  Numeric vector containing the baseline-corrected signal (original -
  baseline)

- original:

  Numeric vector containing the original input signal
