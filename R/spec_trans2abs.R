#' Convert Spectral Data from Transmittance to Absorbance
#'
#' This function converts transmittance data to absorbance using the formula `A = 2 - log10(T)`, where `T` is the transmittance. It also filters out any infinite values resulting from the transformation, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the converted absorbance data, containing the wavelength column and the absorbance numeric columns.
#'
#' @importFrom dplyr select where filter_all all_vars
#' @importFrom recipes recipe step_mutate_at prep bake all_numeric_predictors
#' @importFrom stats as.formula

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
