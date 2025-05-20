#' Normalize Spectral Data to a Specified Range
#'
#' This function normalizes the numeric spectral data in each column to a specified range [min, max], preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param min A numeric value specifying the minimum value of the desired range. Default is 0.
#' @param max A numeric value specifying the maximum value of the desired range. Default is 1.
#'
#' @return A `tibble` with the normalized spectral data, containing the wavelength column and the normalized numeric columns.
#'
#' @importFrom dplyr select where %>%
#' @importFrom recipes recipe step_range prep bake all_numeric_predictors
#' @importFrom stats as.formula
#'
#' @export
spec_norm_minmax <- function(.data, wn_col = NULL, min = 0, max = 1) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (!is.numeric(min) || !is.numeric(max)) {
    stop("Arguments 'min' and 'max' must be numeric values.")
  }

  if (min >= max) {
    stop(glue::glue("The value of 'min' ({min}) must be less than 'max' ({max})."))
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    }
  }

  if (!wn_col %in% names(.data)) {
    stop(glue::glue("Column '{wn_col}' was not found in the provided data."))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(glue::glue("Column '{wn_col}' must contain numeric values."))
  }

  numeric_cols <- dplyr::select(.data, -{{wn_col}}, dplyr::where(is.numeric))
  if (ncol(numeric_cols) == 0) {
    warning("No numeric columns found for normalization. Only the wavenumber column will be returned.")
    return(dplyr::select(.data, {{wn_col}}))
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  recipes::recipe(formula = fmla, data = .data) %>%
    recipes::step_range(recipes::all_numeric_predictors(), min = min, max = max) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
