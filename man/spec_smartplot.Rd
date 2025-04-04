% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec_smartplot.R
\name{spec_smartplot}
\alias{spec_smartplot}
\title{Create a Custom Plot for Spectral Data}
\usage{
spec_smartplot(
  .data,
  wn_col = NULL,
  type = c("absorbance", "transmittance"),
  xdir = c("reverse", "standard"),
  geom = c("point", "line"),
  xmin = NULL,
  xmax = NULL,
  alpha = 0.8
)
}
\arguments{
\item{.data}{A `data.frame` or `tibble` containing spectral data.}

\item{wn_col}{A character string specifying the column name for the wavelength or wavenumber data. This parameter is required.}

\item{type}{A character string specifying the type of data to plot. Choices are `"absorbance"` or `"transmittance"`.}

\item{xdir}{A character string specifying the direction of the x-axis. Choices are `"reverse"` for reverse direction (typically used for wavenumber) or `"standard"` for standard direction.}

\item{geom}{A character string specifying the geometry of the plot. Choices are `"point"` for a scatter plot or `"line"` for a line plot.}

\item{xmin}{A numeric value specifying the minimum x-axis value for the plot. If not provided, the minimum value from the `wn_col` data will be used.}

\item{xmax}{A numeric value specifying the maximum x-axis value for the plot. If not provided, the maximum value from the `wn_col` data will be used.}

\item{alpha}{A numeric value specifying the transparency level of the plotted points or lines. Default is 0.8.}
}
\value{
A `ggplot` object representing the customized spectral plot (absorbance or transmittance as a function of wavelength/wavenumber).
}
\description{
This function generates a customizable plot for spectral data, allowing for the selection of plot type (absorbance or transmittance), x-axis direction, and plot geometry (points or lines).
}
