#' Normalize the Spectra Data by Feature Scaling
#'
#' Normalize the Spectra Data by Feature Scaling so that all the predictors have mean = 0 and variance = 1.
#'
#' @param .data A data.frame that contains the Spectra Data.
#' @param wn_col The name of the column that contains the wavenumber of the spectra data.
#' @return A normalized Spectra Data.
#' @import dplyr
#' @import recipes
#' @import rlang
#' @export
#' @examples

spec_norm_var <- function(.data,wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))


  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_scale(all_numeric_predictors()) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
