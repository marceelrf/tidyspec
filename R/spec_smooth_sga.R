#' Smooth spectra using the S-Golay filter
#'
#' This function smooths spectra using the S-Golay filter. It smooths the data
#' by taking the local polynomial regression of the data. The polynomial
#' regression is performed with the S-Golay filter, which is a
#' least-squares fit of the data to a polynomial over a sliding window of
#' `window` points.
#'
#' @param .data A data frame with spectra data, where the first column is
#'   the wavenumber (Wn) column.
#' @param wn_col The name of the wavenumber column in the data frame.
#'   Default is "Wn".
#' @param window The size of the sliding window for the S-Golay filter.
#'   The size of the window must be an odd number.
#'   Default is 15.
#' @param forder The order of the polynomial to fit in the S-Golay filter.
#'   The order must be smaller than the window size.
#'   Default is 4.
#' @param degree The degree of the polynomial to fit in the S-Golay filter.
#'   The degree must be less than or equal to the forder.
#'   Default is 0.
#' @return A data frame with the smoothed spectra, where the first column is
#'   the wavenumber column and the rest of the columns are the smoothed
#'   spectra values.
#' @importFrom signal sgolayfilt
#' @importFrom recipes recipe step_mutate_at prep bake
#' @importFrom rlang as.formula
#' @importFrom dplyr select
#' @export
#' @examples
#' # Load example spectra data
#' data(spectra_data)
#'
#' # Smooth spectra using the S-Golay filter
#' smooth_spectra <- spec_smooth_sga(spectra_data)
#'
#' # Plot the smoothed spectra
#' plot(smooth_spectra)
#' ```

spec_smooth_sga <- function(.data, wn_col = "Wn", window = 15,forder = 4,degree = 0){
  require(recipes)
  require(signal)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))



  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_mutate_at(all_numeric_predictors(),
                   fn = function(x) sgolayfilt(x = x,
                                               p = forder,
                                               n = window,
                                               m = degree)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
