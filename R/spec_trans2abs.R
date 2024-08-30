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

spec_trans2abs <- function(.data, wn_col = "Wn") {


  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_mutate_at(recipes::all_numeric_predictors(),
                            fn = ~ 2 - log10(.)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter_all(dplyr::all_vars(!is.infinite(.)))
}
