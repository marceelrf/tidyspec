% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spec_select.R
\name{spec_select}
\alias{spec_select}
\title{Select Specific Columns in a Spectral Data Frame}
\usage{
spec_select(.data, ...)
}
\arguments{
\item{.data}{A data frame containing spectral data.}

\item{...}{Column selection helpers (e.g., column names, -column_to_exclude).}
}
\value{
A data frame containing the selected columns.
}
\description{
This function selects user-specified columns from a spectral dataset, always ensuring that the wavenumber column (`wn_col`) is included, unless explicitly excluded.
}
\seealso{
[dplyr::select()], [set_spec_wn()]
}
