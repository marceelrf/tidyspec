#' Normalize Spectra Data to [0,1] Range
#'
#' @param .data data.frame, the input data set
#' @param wn_col character, the name of the column representing the Wavenumber, default is "Wn"
#'
#' @return a data.frame with normalized values of the predictors in [0,1] range, Wn column is preserved.
#'
#' @import dplyr
#' @import recipes
#' @import rlang
#'
#' @export
#'

spec_norm_01 <- function(.data, wn_col = "Wn") {

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  .data %>%
    recipes::recipe(formula = fmla, data = .) %>%
    recipes::step_range(recipes::all_numeric_predictors()) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric))
}
