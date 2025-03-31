#' Normalize Spectral Data to the [0, 1] Range
#'
#' This function normalizes the numeric spectral data in each column to the [0, 1] range, preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the normalized spectral data, containing the wavelength column and the normalized numeric columns.
#'
#' @importFrom dplyr select where %>%
#' @importFrom recipes recipe step_range prep bake all_numeric_predictors
#' @importFrom stats as.formula
#' @export
spec_norm_01 <- function(.data, wn_col = NULL) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_range(recipes::all_numeric_predictors()) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
