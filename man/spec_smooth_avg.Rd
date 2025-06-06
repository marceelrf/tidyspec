% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec_smooth_avg.R
\name{spec_smooth_avg}
\alias{spec_smooth_avg}
\title{Apply Smoothing to Spectral Data Using a Moving Average}
\usage{
spec_smooth_avg(.data, wn_col = NULL, window = 15, degree = 2)
}
\arguments{
\item{.data}{A `data.frame` or `tibble` containing spectral data.}

\item{wn_col}{A character string specifying the column name for the wavelength data. Default is `"Wn"`.}

\item{window}{A numeric value specifying the window size for the moving average smoothing. Default is 15.}

\item{degree}{A numeric value specifying the degree of the polynomial for smoothing. Default is 2.}
}
\value{
A `tibble` with the smoothed spectral data, containing the wavelength column and the smoothed numeric columns.
}
\description{
This function applies a moving average smoothing to numeric spectral data using a specified window size and polynomial degree, while preserving the wavelength column.
}
