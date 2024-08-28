#' Convert Transmittance Spectra to Absorbance Spectra
#'
#' Given a data frame with transmittance spectra, this function converts it to
#' absorbance spectra using the formula: \code{Absorbance = 2 - log10(Transmittance)}.
#' The result is a new data frame with the same wavenumber column and absorbance values.
#'
#' @param .data A data frame with the spectra data. The first column should be the wavenumber column.
#' @param wn_col A string specifying the name of the wavenumber column. Default is "Wn".
#'
#' @return A data frame with the same wavenumber column and absorbance values in
#'   place of the transmittance values. Infinite values are filtered out.
#'
#' @importFrom recipes recipe step_mutate_at prep bake
#' @importFrom rlang as.formula
#' @importFrom dplyr select filter_all all_vars
#' @export
#' @examples
#' # Load example spectra data
#' data(spectra_data)
#'
#' # Convert transmittance spectra to absorbance
#' absorbance_spectra <- spec_trans2abs(spectra_data, wn_col = "Wn")
#'
#' # View the result
#' head(absorbance_spectra)
#'
spec_trans2abs <- function(.data, wn_col = "Wn") {
  require(recipes)
  require(rlang)
  require(dplyr)

  fmla <- as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipe(formula = fmla, data = .) %>%
    step_mutate_at(all_numeric_predictors(), fn = ~ 2 - log10(.)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}}, where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}
