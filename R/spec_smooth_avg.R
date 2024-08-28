#' Perform a Smoothed Average on the Spectra Data
#'
#' This function calculates a smoothed average of the spectral data using a specified window size and polynomial degree. The smoothed average is useful for reducing noise in the spectra.
#'
#' @param .data A data frame containing the spectra data.
#' @param wn_col A character string representing the column name of the wavenumber.
#' @param window An integer value specifying the window size to be used in the smoothed average calculation.
#' @param degree An integer value specifying the polynomial degree to be used in the smoothed average calculation.
#' @return A data frame containing the wavenumber column and the smoothed average values of the spectra data.
#' @import recipes
#' @import dplyr
#' @import timetk
#' @export
#' @examples
#' spec_smooth_avg(.data = spectra_data, wn_col = "Wn", window = 15, degree = 2)
spec_smooth_avg <- function(.data, wn_col = "Wn", window = 15, degree = 2) {
  require(recipes)
  require(timetk)
  require(rlang)
  require(dplyr)

  fmla <- as.formula(paste({{wn_col}}," ~ .", sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_smooth(all_numeric_predictors(),
                period = window,
                degree = degree) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}}, where(is.numeric))
}
