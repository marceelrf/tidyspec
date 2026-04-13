# Package index

## Package Setup

Functions to configure tidyspec environment

- [`set_spec_wn()`](https://marceelrf.github.io/tidyspec/reference/set_spec_wn.md)
  : Set the Default Wavenumber Column
- [`check_wn_col()`](https://marceelrf.github.io/tidyspec/reference/check_wn_col.md)
  : Check the Currently Set Wavenumber Column

## Data Import

Functions to read spectral data

- [`spec_read()`](https://marceelrf.github.io/tidyspec/reference/spec_read.md)
  : Read Spectral Data from Various File Formats

## Data Transformation

Functions to transform spectral data

- [`spec_trans2abs()`](https://marceelrf.github.io/tidyspec/reference/spec_trans2abs.md)
  : Convert Spectral Data from Transmittance to Absorbance
- [`spec_abs2trans()`](https://marceelrf.github.io/tidyspec/reference/spec_abs2trans.md)
  : Convert Absorbance Data to Transmittance
- [`spec_diff()`](https://marceelrf.github.io/tidyspec/reference/spec_diff.md)
  : Apply Differentiation to Spectral Data
- [`spec_filter()`](https://marceelrf.github.io/tidyspec/reference/spec_filter.md)
  : Filter spectral data by wavenumber range
- [`spec_select()`](https://marceelrf.github.io/tidyspec/reference/spec_select.md)
  : Select Specific Columns in a Spectral Data Frame
- [`spec_norm_01()`](https://marceelrf.github.io/tidyspec/reference/spec_norm_01.md)
  : Normalize Spectral Data to the \[0, 1\] Range
- [`spec_norm_minmax()`](https://marceelrf.github.io/tidyspec/reference/spec_norm_minmax.md)
  : Normalize Spectral Data to a Specified Range
- [`spec_norm_var()`](https://marceelrf.github.io/tidyspec/reference/spec_norm_var.md)
  : Standardize Spectral Data to Unit Variance
- [`spec_norm_area()`](https://marceelrf.github.io/tidyspec/reference/spec_norm_area.md)
  : Normalize Spectral Data by Area Under the Curve
- [`spec_norm_by_wn()`](https://marceelrf.github.io/tidyspec/reference/spec_norm_by_wn.md)
  : Normalize Spectral Data by Specific Wavenumber Value
- [`spec_smooth_avg()`](https://marceelrf.github.io/tidyspec/reference/spec_smooth_avg.md)
  : Apply Smoothing to Spectral Data Using a Moving Average
- [`spec_smooth_sga()`](https://marceelrf.github.io/tidyspec/reference/spec_smooth_sga.md)
  : Apply Savitzky-Golay Smoothing to Spectral Data
- [`spec_values_at()`](https://marceelrf.github.io/tidyspec/reference/spec_values_at.md)
  : Extract spectral values at specific wavenumbers
- [`spec_join()`](https://marceelrf.github.io/tidyspec/reference/spec_join.md)
  : Join multiple spectral tibbles

## Baseline Correction

Functions for baseline correction and extraction

- [`spec_blc_rollingBall()`](https://marceelrf.github.io/tidyspec/reference/spec_blc_rollingBall.md)
  : Apply Rolling Ball Baseline Correction to Spectral Data
- [`spec_blc_rubberband()`](https://marceelrf.github.io/tidyspec/reference/spec_blc_rubberband.md)
  : Apply Rubberband Baseline Correction to Spectral Data
- [`spec_bl_rubberband()`](https://marceelrf.github.io/tidyspec/reference/spec_bl_rubberband.md)
  : Extract Rubberband Baseline from Spectral Data
- [`spec_bl_rollingBall()`](https://marceelrf.github.io/tidyspec/reference/spec_bl_rollingBall.md)
  : Extract Rolling Ball Baseline from Spectral Data
- [`demo_rolling_ball()`](https://marceelrf.github.io/tidyspec/reference/demo_rolling_ball.md)
  : Usage Example and Test
- [`plot_rolling_ball()`](https://marceelrf.github.io/tidyspec/reference/plot_rolling_ball.md)
  : Plot Rolling Ball Results
- [`rolling_ball()`](https://marceelrf.github.io/tidyspec/reference/rolling_ball.md)
  : Rolling Ball Baseline Correction
- [`rolling_ball_morphology()`](https://marceelrf.github.io/tidyspec/reference/rolling_ball_morphology.md)
  : Enhanced Rolling Ball with Mathematical Morphology
- [`smooth_baseline()`](https://marceelrf.github.io/tidyspec/reference/smooth_baseline.md)
  : Baseline Smoothing

## Interpolation and Resampling

Functions for spectral interpolation and grid alignment

- [`spec_interpolate_left()`](https://marceelrf.github.io/tidyspec/reference/spec_interpolate_left.md)
  : Interpolate Spectral Data to Match Reference Wavenumbers (Left Join
  Style)
- [`spec_interpolate_right()`](https://marceelrf.github.io/tidyspec/reference/spec_interpolate_right.md)
  : Interpolate Spectral Data to Include Both Grids (Right Join Style)
- [`spec_interpolate_regular()`](https://marceelrf.github.io/tidyspec/reference/spec_interpolate_regular.md)
  : Interpolate to Regular Grid

## Plotting and Visualization

Functions for plotting spectral data

- [`spec_smartplot()`](https://marceelrf.github.io/tidyspec/reference/spec_smartplot.md)
  : Create a Custom Plot for Spectral Data
- [`spec_smartplotly()`](https://marceelrf.github.io/tidyspec/reference/spec_smartplotly.md)
  : Create an Interactive Plot for Spectral Data using Plotly

## PCA

Principal Component Analysis for spectral data

- [`spec_pca()`](https://marceelrf.github.io/tidyspec/reference/spec_pca.md)
  : Perform Principal Component Analysis (PCA) on Spectral Data
- [`spec_pca_screeplot()`](https://marceelrf.github.io/tidyspec/reference/spec_pca_screeplot.md)
  : Scree plot for PCA results
- [`spec_pca_wn_contrib()`](https://marceelrf.github.io/tidyspec/reference/spec_pca_wn_contrib.md)
  : Compute Wavenumber Contributions to Principal Components

## Data

- [`CoHAspec`](https://marceelrf.github.io/tidyspec/reference/CoHAspec.md)
  : CoHAspec Dataset
