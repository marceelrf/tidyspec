#' Normalize Spectral Data to the [0, 1] Range
#'
#' This function normalizes the numeric spectral data in each column to the [0, 1] range, preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the normalized spectral data, containing the wavelength column and the normalized numeric columns.
#'
#' @importFrom dplyr select where
#' @importFrom recipes recipe step_range prep bake all_numeric_predictors
#' @importFrom stats as.formula

spec_norm_01 <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_range(recipes::all_numeric_predictors()) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
