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


spec_norm_var <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_scale(recipes::all_numeric_predictors()) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
