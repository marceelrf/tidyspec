#' Perform Baseline Estimation Using IRLS Method
#'
#' This function estimates the baseline of spectral data using the Iteratively Reweighted Least Squares (IRLS) method.
#' The baseline correction can be applied to absorbance or transmittance spectra depending on the `is_abs` parameter.
#'
#' @param .data A data frame or tibble containing the spectral data.
#' @param wn_col A character string specifying the column name that contains the wavelength or wavenumber values. Default is `"Wn"`.
#' @param Wn_min Numeric value indicating the minimum wavenumber to filter the data.
#' @param Wn_max Numeric value indicating the maximum wavenumber to filter the data.
#' @param lambda1 A numeric value for the 2nd derivative constraint for primary smoothing in the IRLS method.
#' @param lambda2 A numeric value for the 2nd derivative constraint for secondary smoothing in the IRLS method.
#' @param maxit An integer specifying the maximum number of iterations for the IRLS method. Default is 200.
#' @param wi A numeric value representing the initial weight for the IRLS algorithm. Default is 0.05.
#' @param is_abs A logical value indicating whether the input data is in absorbance (`TRUE`) or transmittance (`FALSE`). Default is `TRUE`.
#'
#' @return A tibble with the estimated baseline of the spectral data, including the wavenumber column.
#'
#' @importFrom dplyr select mutate where %>%
#' @importFrom baseline baseline
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
#'
#' @references
#' Baseline estimation performed using the `baseline` package for R.
#' More information can be found at: \url{https://CRAN.R-project.org/package=baseline}
#'
#' @seealso [baseline::baseline()], [spec_trans2abs()]
#'
#' @export

spec_bl_irls <- function(.data,
                                wn_col = "Wn",
                                Wn_min,
                                Wn_max,
                         lambda1,
                         lambda2,
                         maxit = 200,
                         wi = 0.05,
                                is_abs = TRUE) {
  mat <- .data[.data[[wn_col]] >= Wn_min & .data[[wn_col]] <= Wn_max, ]

  if (is_abs) {
    mat %>%
      dplyr::select(-{{wn_col}}) %>%
      t() %>%
      baseline::baseline(method = "irls",
                         lambda1,
                         lambda2,
                         maxit = 200,
                         wi = 0.05) %>%
      purrr::pluck("baseline") %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{wn_col}} := mat[[wn_col]]) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric))
  } else {
    mat %>%
      spec_trans2abs(wn_col = {{wn_col}}) %>%
      dplyr::select(-{{wn_col}}) %>%
      t() %>%
      baseline::baseline(method = "irls",
                         lambda1,
                         lambda2,
                         maxit = 200,
                         wi = 0.05) %>%
      purrr::pluck("baseline") %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{wn_col}} := mat[[wn_col]]) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric))
  }
}
