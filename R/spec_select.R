#' Select Specific Columns in a Spectral Data Frame
#'
#' This function selects user-specified columns from a spectral dataset, always ensuring that the wavenumber column (`wn_col`) is included.
#'
#' @param .data A data frame containing spectral data.
#' @param wn_col A string specifying the column name that represents the wavenumber values. If NULL, the function will attempt to retrieve a default value set with `set_spec_wn()`.
#' @param ... Additional column names to be selected, provided as unquoted variable names.
#'
#' @return A data frame containing only the selected columns.
#' @export
#'
#' @seealso [dplyr::select()], [set_spec_wn()]

spec_select <- function(.data, wn_col = NULL, ...) {
  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }


  selected_cols <- c(wn_col, sapply(rlang::ensyms(...), rlang::as_name))

  dplyr::select(.data, all_of(selected_cols))
}
