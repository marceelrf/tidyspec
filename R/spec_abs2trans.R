#' Convert Absorbance Spectra to Transmittance Spectra
#'
#' Converts a spectra in absorbance to transmittance by applying the formula: T = 10^(2-A).
#'
#' @param .data A data.frame or tibble containing the absorbance spectra.
#' @param wn_col The name of the column in the data containing the wavenumber. Default is "Wn".
#' @return A tibble containing the wavenumber and corresponding transmittance spectra.
#' @examples
#' # Load example data
#' library(tidyspec)
#' data(spectra_abs)
#'
#' # Convert to transmittance
#' spectra_trans <- spec_abs2trans(spectra_abs)
#'
#' @import recipes
#' @import rlang
#' @import dplyr
#' @export

spec_abs2trans <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste({{wn_col}}, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_mutate_at(recipes::all_predictors(), fn = ~ 10^(2 - .)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter_all(dplyr::all_vars(!is.infinite(.)))
}
