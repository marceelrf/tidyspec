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

spec_abs2trans <- function(.data, wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))
  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_mutate_at(all_predictors(), fn = function(x) 10^(2-x)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}
