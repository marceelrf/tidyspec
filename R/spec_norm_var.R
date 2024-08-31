#' Standardize Spectral Data to Unit Variance
#'
#' This function standardizes the numeric spectral data in each column to have a mean of 0 and a standard deviation of 1 (unit variance), while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the standardized spectral data, containing the wavelength column and the standardized numeric columns.
#'
#' @importFrom dplyr select where
#' @importFrom recipes recipe step_scale prep bake all_numeric_predictors
#' @importFrom stats as.formula


spec_norm_var <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_scale(recipes::all_numeric_predictors()) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
