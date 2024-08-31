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
#' @importFrom dplyr select where
#' @importFrom recipes recipe step_range prep bake all_numeric_predictors
#' @importFrom stats as.formula

spec_norm_minmax <- function(.data, wn_col = "Wn", min = 0, max = 1) {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_range(recipes::all_numeric_predictors(), min = min, max = max) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
