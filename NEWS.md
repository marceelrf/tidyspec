# tidyspec 0.2.0

- Added a `NEWS.md` file to track changes to the package.

## tidyspec (development version)
### New features

- Added `spec_blc_rubberband()` function for rubberband baseline correction of spectral data
- Added `spec_bl_rubberband()` function to extract rubberband baseline from spectral data
- Rubberband method provides automatic baseline correction by connecting local minima points
- Both functions support customizable segment length and baseline smoothing options

### Improvements

- Enhanced baseline correction options with rubberband algorithm alongside existing rolling ball method
- Consistent API design between rolling ball and rubberband baseline functions
