#' Apply Differentiation to Spectral Data
#'
#' This function applies numerical differentiation to spectral data, allowing for the calculation of the first or higher-order differences.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param degree A numeric value specifying the degree of differentiation. If `degree` is 0, the original data is returned without any changes.
#'
#' @return A `tibble` with the differentiated spectral data, containing the wavelength column and the differentiated numeric columns. If `degree` is 0, the original data is returned.
#'
#' @importFrom dplyr select where starts_with %>%
#' @importFrom recipes recipe prep bake all_numeric_predictors
#' @importFrom timetk step_diff
#' @importFrom stats as.formula
#' @importFrom rlang :=
#'
#' @export
spec_diff <- function(.data, wn_col = NULL, degree = 1) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  if (degree == 0) {
    return(.data)
  } else {

    recipes::recipe(formula = fmla, data =  .data) %>%
      timetk::step_diff(recipes::all_numeric_predictors(), difference = degree, lag = 1) %>%
      recipes::prep() %>%
      recipes::bake(NULL) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
      dplyr::select({{wn_col}}, dplyr::starts_with("diff"))
  }
}
