#' Title: Spectral Derivative
#'
#' @description
#' The `spec_diff` function calculates the derivative of a spectral data set. The default derivative degree is 1, but this can be changed to any positive integer. The resulting data set will contain columns for each derivative and the Wn column.
#'
#' @param .data A tibble or data.frame containing the spectral data.
#' @param wn_col Character string representing the column name for the Wn data.
#' @param degree Numeric value for the derivative degree, default is 1.
#' @return A tibble or data.frame with columns for each derivative of the input data and the Wn column.
#' @import recipes
#' @import timetk
#' @import rlang
#' @import dplyr
#' @export
#'

spec_diff <- function(.data, wn_col = "Wn", degree = 1) {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  if (degree == 0) {
    return(.data)
  } else {
    .data %>%
      recipes::recipe(formula = fmla, data = .) %>%
      timetk::step_diff(recipes::all_numeric_predictors(), difference = degree, lag = 1) %>%
      recipes::prep() %>%
      recipes::bake(NULL) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
      dplyr::select({{wn_col}}, dplyr::starts_with("diff"))
  }
}
