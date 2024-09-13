#' Convert Absorbance Data to Transmittance
#'
#' This function converts absorbance data to transmittance using the formula \eqn{T = 10^{(2 - A)}}, where \eqn{A} is the absorbance and \eqn{T} is the transmittance.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data in absorbance.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the converted transmittance data, containing the wavelength column and the numeric transmittance columns. Any rows with infinite values are removed.
#'
#' @importFrom dplyr select where filter_all all_vars %>%
#' @importFrom recipes recipe step_mutate_at prep bake all_predictors
#' @importFrom stats as.formula
#' @export
spec_abs2trans <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_mutate_at(recipes::all_predictors(), fn = ~ 10^(2 - .)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter_all(dplyr::all_vars(!is.infinite(.)))
}
