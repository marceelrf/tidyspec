# tidyspec 0.4.0

## New functions

### `spec_join()`
* **Join multiple spectral tibbles**: Combine two or more spectral datasets that share the same wavenumber column
* **Flexible join types**: Support for "full" (default), "inner", and "left" joins
* **Conflict handling**: Automatic suffix handling for duplicate column names
* **Validation**: Comprehensive checks for input tibbles and wavenumber column consistency
* **Integration**: Full compatibility with the tidyspec workflow and `set_spec_wn()` global settings

### `spec_values_at()`
* **Extract values at specific wavenumbers**: Retrieve spectral intensities at user-defined wavenumber positions
* **Multiple extraction methods**:
  - `"closest"`: Find nearest wavenumber matches (default)
  - `"exact"`: Return only exact matches, NA for non-matches
  - `"interpolate"`: Use linear interpolation between adjacent data points
* **Flexible output formats**:
  - `"wide"`: Wavenumbers as rows, samples as columns (default)
  - `"long"`: Tidy format with wavenumber, sample, and value columns
* **Tolerance control**: Set maximum allowed difference for closest matching
* **Use cases**: Ideal for peak analysis, sample comparison, and creating datasets for statistical analysis

## Enhancements

* Both new functions follow tidyspec conventions with consistent parameter naming and behavior
* Integration with the global wavenumber column system (`set_spec_wn()`)
* Comprehensive input validation and informative error messages
* Support for the standard tidyspec warning system for parameter defaults

## Documentation

* Added comprehensive documentation with examples for both new functions
* Detailed parameter descriptions and method explanations
* Usage examples covering common spectroscopy workflows

# tidyspec 0.3.0

## New Features

### Spectral Interpolation Functions
* Added `spec_interpolate_left()` for interpolating spectral data to match reference wavenumber grids (left join style)
* Added `spec_interpolate_right()` for combining datasets with interpolated reference data (right join style)
* Added `spec_interpolate_regular()` for creating regular wavenumber grids with specified resolution
* Support for multiple interpolation methods:
  - `"linear"` - Linear interpolation (base R)
  - `"pchip"` - Piecewise Cubic Hermite Interpolating Polynomial (requires `pracma`)
  - `"spline"` - Cubic spline interpolation (base R)
  - `"akima"` - Akima spline interpolation (requires `akima`)
* Optional extrapolation control with `extrapolate` parameter
* Automatic handling of overlapping column names with customizable suffixes
* Consistent integration with tidyspec environment for automatic wavenumber column detection

## Dependencies
* Added `pracma` and `akima` to Suggests for advanced interpolation methods
* Functions gracefully handle missing optional dependencies with informative error messages

## Documentation
* Comprehensive documentation for all interpolation functions with usage examples
* Clear explanation of join-style behavior for spectral data alignment

# tidyspec 0.2.0

## New Features

### Baseline Correction Functions
* Added `spec_blc_rubberband()` function for rubberband baseline correction of spectral data
* Added `spec_bl_rubberband()` function to extract rubberband baseline from spectral data
* Rubberband method provides automatic baseline correction by connecting local minima points
* Both functions support customizable segment length and baseline smoothing options

## Improvements
* Enhanced baseline correction options with rubberband algorithm alongside existing rolling ball method
* Consistent API design between rolling ball and rubberband baseline functions

## Documentation
* Added a `NEWS.md` file to track changes to the package
