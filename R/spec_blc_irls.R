#' Perform Baseline Correction Using IRLS Method
#'
#' This function applies a baseline correction to spectral data using the Iteratively Reweighted Least Squares (IRLS) method.
#' The baseline correction can be applied to absorbance or transmittance spectra depending on the `is_abs` parameter.
#'
#' @param .data A data frame or tibble containing the spectral data.
#' @param wn_col A character string specifying the column name that contains the wavelength or wavenumber values. Default is `"Wn"`.
#' @param wn_min Numeric value indicating the minimum wavenumber to filter the data.
#' @param wn_max Numeric value indicating the maximum wavenumber to filter the data.
#' @param lambda1 A numeric value for the 2nd derivative constraint for primary smoothing in the IRLS method.
#' @param lambda2 A numeric value for the 2nd derivative constraint for secondary smoothing in the IRLS method.
#' @param maxit An integer specifying the maximum number of iterations for the IRLS method. Default is 200.
#' @param wi A numeric value representing the initial weight for the IRLS algorithm. Default is 0.05.
#' @param is_abs A logical value indicating whether the input data is in absorbance (`TRUE`) or transmittance (`FALSE`). Default is `TRUE`.
#'
#' @return A tibble with the baseline-corrected spectral data, including the wavenumber column.
#'
#' @importFrom dplyr select mutate where %>%
#' @importFrom baseline baseline
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
#'
#' @references
#' Baseline correction performed using the `baseline` package for R.
#' More information can be found at: \url{https://CRAN.R-project.org/package=baseline}
#'
#' @seealso [baseline::baseline()], [spec_trans2abs()]
#'
#' @export


spec_blc_irls <- function(.data,
                          wn_col = NULL,
                          wn_min,
                          wn_max,
                          lambda1,
                          lambda2,
                          maxit = 200,
                          wi = 0.05,
                          is_abs = TRUE) {
  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }


  if (missing(lambda1) || missing(lambda2)) {
    stop("ws and wm are mandatory arguments.")
  }

  wn_values <- .data[[wn_col]]

  if (is.null(wn_min)) {
    wn_min <- min(wn_values)
  }
  if (is.null(wn_max)) {
    wn_max <- max(wn_values)
  }

  mat <- .data[.data[[wn_col]] >= wn_min & .data[[wn_col]] <= wn_max, ]

  if (is_abs) {
    mat %>%
      dplyr::select(-{{wn_col}}) %>%
      t() %>%
      baseline::baseline(method = "irls",
                         lambda1 = lambda1,
                         lambda2 = lambda2,
                         maxit = maxit,
                         wi = wi) %>%
      purrr::pluck("corrected") %>%
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
                         lambda1 = lambda1,
                         lambda2 = lambda2,
                         maxit = maxit,
                         wi =wi) %>%
      purrr::pluck("corrected") %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{wn_col}} := mat[[wn_col]]) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric))
  }
}
