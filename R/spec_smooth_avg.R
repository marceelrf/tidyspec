#' Perform a smoothed average on the spectra data
#'
#' @param .data A data frame or matrix that contains the spectra data.
#' @param wn_col A character string that represents the column name of the wavenumber.
#' @param window An integer value that represents the window size to be used in the smoothed average calculation.
#' @param degree An integer value that represents the polynomial degree to be used in the smoothed average calculation.
#'
#' @return A matrix or data frame that contains the wavenumber column and smoothed average values of the spectra data.
#'
#' @import recipes
#' @import dplyr
#' @import timetk
#'
#' @examples
#' spec_smooth_avg(.data = spectra_data, wn_col = "Wn", window = 15, degree = 2)
#'
#' @export
#'

spec_smooth_avg <- function(.data, wn_col= "Wn", window = 15, degree = 2){
  require(recipes)
  require(timetk)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_smooth(all_numeric_predictors(),
                period = window,
                degree = degree) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
