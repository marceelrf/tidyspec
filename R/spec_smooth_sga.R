#' Apply Savitzky-Golay Smoothing to Spectral Data
#'
#' This function applies Savitzky-Golay smoothing to numeric spectral data using a specified window size, polynomial order, and differentiation degree, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param window A numeric value specifying the window size for the Savitzky-Golay smoothing. Default is 15.
#' @param forder A numeric value specifying the polynomial order for smoothing. Default is 4.
#' @param degree A numeric value specifying the degree of differentiation. Default is 0 (no differentiation).
#'
#' @return A `tibble` with the smoothed spectral data, containing the wavelength column and the smoothed numeric columns.
#'
#' @importFrom dplyr select where
#' @importFrom recipes recipe step_mutate_at prep bake all_numeric_predictors
#' @importFrom stats as.formula
#' @importFrom signal sgolayfilt

spec_smooth_sga <- function(.data, wn_col = "Wn", window = 15, forder = 4, degree = 0) {


  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_mutate_at(recipes::all_numeric_predictors(),
                   fn = function(x) signal::sgolayfilt(x = x, p = forder, n = window, m = degree)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, where(is.numeric))
}
