#' Apply Rolling Ball Baseline Correction to Spectral Data
#'
#' This function applies a rolling ball baseline correction to spectral data within a specified wavelength range.
#' It allows for correction of either absorbance or transmittance data.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param Wn_min A numeric value specifying the minimum wavelength to consider for the baseline correction.
#' @param Wn_max A numeric value specifying the maximum wavelength to consider for the baseline correction.
#' @param wm A numeric value for the window size of the rolling ball algorithm.
#' @param ws A numeric value for the smoothing factor of the rolling ball algorithm.
#' @param is_abs A logical value indicating whether the data is already in absorbance. If `TRUE`, absorbance is used directly; if `FALSE`, the data is converted to absorbance before applying the baseline correction.
#'
#' @return A `tibble` with the baseline-corrected spectral data, containing the wavelength column and the corrected numeric columns.
#'
#' @importFrom dplyr select mutate where
#' @importFrom baseline baseline
#' @importFrom purrr pluck
#' @importFrom tibble as_tibble
#'
#' @references
#' Baseline estimation performed using the `baseline` package for R.
#' More information can be found at: \url{https://CRAN.R-project.org/package=baseline}
#'
spec_blc_rollingBall <- function(.data,
                                 wn_col = "Wn",
                                 Wn_min,
                                 Wn_max,
                                 wm,
                                 ws,
                                 is_abs = TRUE) {

  mat <- .data[.data[[wn_col]] >= Wn_min & .data[[wn_col]] <= Wn_max, ]

  if (is_abs) {
    mat %>%
      dplyr::select(-{{wn_col}}) %>%
      t() %>%
      baseline::baseline(method = "rollingBall", wm = wm, ws = ws) %>%
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
      baseline::baseline(method = "rollingBall", wm = wm, ws = ws) %>%
      purrr::pluck("corrected") %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{wn_col}} := mat[[wn_col]]) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric))
  }
}
