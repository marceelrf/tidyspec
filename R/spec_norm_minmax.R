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

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  recipes::recipe(formula = fmla, data = .data) %>%
    recipes::step_range(recipes::all_numeric_predictors(), min = min, max = max) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
