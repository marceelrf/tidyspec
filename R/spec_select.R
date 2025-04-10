#' Select Specific Columns in a Spectral Data Frame
#'
#' This function selects user-specified columns from a spectral dataset, always ensuring that the wavenumber column (`wn_col`) is included, unless explicitly excluded.
#'
#' @param .data A data frame containing spectral data.
#' @param ... Column selection helpers (e.g., column names, -column_to_exclude).
#'
#' @return A data frame containing the selected columns.
#' @export
#'
#' @seealso [dplyr::select()], [set_spec_wn()]
spec_select <- function(.data, ...) {
  wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
  if (is.null(wn_col)) {
    stop("wn_col not specified and no pattern defined with set_spec_wn()")
  }

  dplyr::select(.data, {{ wn_col }}, ...)
}
