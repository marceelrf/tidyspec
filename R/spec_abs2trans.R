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
#' @importFrom tidyselect everything
#' @importFrom recipes recipe step_mutate_at prep bake all_predictors
#' @importFrom stats as.formula
#' @export
spec_abs2trans <- function(.data, wn_col = NULL) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  recipes::recipe(formula = fmla, data = .data) %>%
    recipes::step_mutate_at(recipes::all_predictors(),
                            fn = \(x) 10^(2 - x)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter(dplyr::across(tidyselect::everything(), ~ !is.infinite(.)))
}
