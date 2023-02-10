#' Normalize Spectra Data to [0,1] Range
#'
#' @param .data data.frame, the input data set
#' @param wn_col character, the name of the column representing the Wavenumber, default is "Wn"
#'
#' @return a data.frame with normalized values of the predictors in [0,1] range, Wn column is preserved.
#'
#' @examples
#'
#' \dontrun{
#' data(iris)
#' norm_iris <- spec_norm_01(iris)
#' }
#'
#' @import dplyr
#' @import recipes
#' @import rlang
#'
#' @export
#'

spec_norm_01 <- function(.data,wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_range(all_numeric_predictors()) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
