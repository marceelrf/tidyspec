# Usage Example and Test

Function to demonstrate the use of implemented functions

## Usage

``` r
demo_rolling_ball(verbose = TRUE)
```

## Arguments

- verbose:

  Logical indicating whether to print progress messages

## Value

A list containing two elements:

- simple:

  Results from the simple rolling ball method

- morphology:

  Results from the mathematical morphology method

## Examples

``` r
# \donttest{
# Run the demonstration
demo_results <- demo_rolling_ball()
#> Rolling Ball Baseline Correction Demonstration
#> ============================================
#> 
#> Applying Rolling Ball (simple method)...
#> Applying Rolling Ball (mathematical morphology)...


#> 
#> Demonstration completed!
#> Try different wm/radius values to optimize the results.

# Access results
simple_method <- demo_results$simple
morphology_method <- demo_results$morphology
# }
```
