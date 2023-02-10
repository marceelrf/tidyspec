#' Convert transmittance spectra to absorbance spectra
#'
#' Given a data frame with transmittance spectra, this function converts it to
#' absorbance spectra. The result is a new data frame with the same wavenumber
#' column and absorbance values in place of the transmittance values.
#'
#' @param .data A data frame with the spectra data
#' @param wn_col A string specifying the name of the wavenumber column
#'
#' @return A data frame with the same wavenumber column and absorbance values in
#' place of the transmittance values. Infinite values are filtered out.
#'
#' @examples
#'
#' spec_trans2abs(.data = spectra_data, wn_col = "Wn")
#'
#' @export
#'

spec_trans2abs <- function(.data, wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_mutate_at(all_predictors(), fn = function(x) 2-log10(x)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}
