#' Apply Smoothing to Spectral Data Using a Moving Average
#'
#' This function applies a moving average smoothing to numeric spectral data using a specified window size and polynomial degree, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param window A numeric value specifying the window size for the moving average smoothing. Default is 15.
#' @param degree A numeric value specifying the degree of the polynomial for smoothing. Default is 2.
#'
#' @return A `tibble` with the smoothed spectral data, containing the wavelength column and the smoothed numeric columns.
#'
#' @importFrom dplyr select where %>%
#' @importFrom recipes recipe prep bake all_numeric_predictors
#' @importFrom stats as.formula
#' @importFrom timetk step_smooth
#'
#' @export
spec_smooth_avg <- function(.data, wn_col = NULL, window = 15, degree = 2) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }


  fmla <- stats::as.formula(paste(wn_col," ~ .", sep = ""))

  recipes::recipe(formula = fmla, data = .data) %>%
    timetk::step_smooth(recipes::all_numeric_predictors(),
                period = window,
                degree = degree) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
