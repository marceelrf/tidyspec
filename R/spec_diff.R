#' Title: Spectral Derivative
#'
#' @description
#' The `spec_diff` function calculates the derivative of a spectral data set. The default derivative degree is 1, but this can be changed to any positive integer. The resulting data set will contain columns for each derivative and the Wn column.
#'
#' @param .data a tibble or data.frame containing the spectral data.
#' @param wn_col character string representing the column name for the Wn data.
#' @param degree numeric value for the derivative degree, default is 1.
#'
#' @return A tibble or data.frame with columns for each derivative of the input data and the Wn column.
#'
#' @examples
#' library(recipes)
#' library(timetk)
#' library(rlang)
#' library(dplyr)
#'
#' spec_diff(.data = iris, wn_col = "Species", degree = 2)
#'
#' @import recipes
#' @import timetk
#' @import rlang
#' @import dplyr
#' @export
#'

spec_diff <- function(.data,wn_col = "Wn", degree=1){
  require(recipes)
  require(timetk)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))


  if(degree == 0) {
    .data
  } else {

    .data %>%
      recipe(formula = fmla,
             data = .) %>%
      step_diff(all_numeric_predictors(),difference = degree,lag = 1) %>%
      prep() %>%
      bake(NULL) %>%
      select({{wn_col}},where(is.numeric)) %>%
      select({{wn_col}}, starts_with("diff"))
  }
}
