#' @title Normalize Data using Min-Max Scaler
#' @description Normalize the numeric predictors using Min-Max Scaler, with custom values of minimum and maximum scaling.
#' @param .data data.frame. The input data.
#' @param wn_col character. The column representing the dependent variable (Wavenumber). Default is "Wn".
#' @param min numeric. The minimum value to be used in scaling. Default is 0.
#' @param max numeric. The maximum value to be used in scaling. Default is 1.
#' @return A data.frame with only the dependent variable column and the normalized predictors.
#' @examples
#' spec_norm_minmax(mtcars, "mpg", 0, 10)
#' @export

spec_norm_minmax <- function(.data, wn_col = "Wn", min = 0, max = 1) {

  fmla <- stats::as.formula(paste({{wn_col}}, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_range(recipes::all_numeric_predictors(), min = min, max = max) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
