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
    stop("wn_col not specified and no default defined with set_spec_wn().")
  }

  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (!(wn_col %in% names(.data))) {
    stop(sprintf("The wavenumber column '%s' was not found in the dataset.", wn_col))
  }

  selected <- tryCatch(
    dplyr::select(.data, {{ wn_col }}, ...),
    error = function(e) {
      stop("Error selecting columns: ", e$message)
    }
  )

  if (ncol(selected) == 0) {
    warning("No columns were selected. Only the wavenumber column is guaranteed.")
  }

  if (!(wn_col %in% names(selected))) {
    warning(sprintf("The wavenumber column '%s' was excluded from the selection. This is not recommended.", wn_col))
  }

  return(selected)
}
