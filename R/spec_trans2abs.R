#' Convert Spectral Data from Transmittance to Absorbance
#'
#' This function converts transmittance data to absorbance using the formula `A = 2 - log10(T)`, where `T` is the transmittance. It also filters out any infinite values resulting from the transformation, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the converted absorbance data, containing the wavelength column and the absorbance numeric columns.
#'
#' @importFrom dplyr select where filter_all all_vars %>%
#' @importFrom tidyselect everything
#' @importFrom recipes recipe step_mutate_at prep bake all_numeric_predictors
#' @importFrom stats as.formula
#'
#' @export

spec_trans2abs <- function(.data, wn_col = NULL) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }


  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  recipes::recipe(formula = fmla, data = .data) %>%
    recipes::step_mutate_at(recipes::all_numeric_predictors(),
                            fn = \(x) 2 - log10(x)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter(dplyr::across(tidyselect::everything(), ~ !is.infinite(.)))
}
