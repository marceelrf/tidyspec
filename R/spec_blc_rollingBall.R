#' Apply Rolling Ball Baseline Correction to Spectral Data
#'
#' This function applies a rolling ball baseline correction to spectral data within a specified wavelength range.
#' It allows for correction of either absorbance or transmittance data.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param wn_min A numeric value specifying the minimum wavelength to consider for the baseline correction.
#' @param wn_max A numeric value specifying the maximum wavelength to consider for the baseline correction.
#' @param wm A numeric value for the window size of the rolling ball algorithm.
#' @param ws A numeric value for the smoothing factor of the rolling ball algorithm.
#' @param is_abs A logical value indicating whether the data is already in absorbance. If `TRUE`, absorbance is used directly; if `FALSE`, the data is converted to absorbance before applying the baseline correction.
#'
#' @return A `tibble` with the baseline-corrected spectral data, containing the wavelength column and the corrected numeric columns.
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
#' @export
spec_blc_rollingBall <- function(.data,
                                 wn_col = NULL,
                                 wn_min = NULL,
                                 wn_max = NULL,
                                 wm,
                                 ws,
                                 is_abs = TRUE) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (missing(wm) || missing(ws)) {
    stop("Arguments 'wm' and 'ws' are required.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    }
  }

  if (!wn_col %in% names(.data)) {
    stop(paste0("Column '", wn_col, "' was not found in '.data'."))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(paste0("Column '", wn_col, "' must contain numeric values."))
  }

  wn_values <- .data[[wn_col]]

  if (is.null(wn_min)) {
    wn_min <- min(wn_values, na.rm = TRUE)
    warning(paste0("'wn_min' was not provided. Using the minimum of column '", wn_col, "': ", wn_min))
  }

  if (is.null(wn_max)) {
    wn_max <- max(wn_values, na.rm = TRUE)
    warning(paste0("'wn_max' was not provided. Using the maximum of column '", wn_col, "': ", wn_max))
  }

  if (wn_min >= wn_max) {
    stop("'wn_min' must be less than 'wn_max'.")
  }

  mat <- .data[.data[[wn_col]] >= wn_min & .data[[wn_col]] <= wn_max, ]

  if (nrow(mat) == 0) {
    stop("No data found in the specified wavenumber range (wn_min to wn_max).")
  }

  num_cols <- setdiff(names(mat), wn_col)
  if (length(num_cols) == 0) {
    stop("No numeric column was found besides 'wn_col'.")
  }

  if (!is.logical(is_abs) || length(is_abs) != 1) {
    stop("The 'is_abs' argument must be TRUE or FALSE.")
  }

  if (!is.numeric(wm) || wm <= 0) {
    warning("'wm' should be a positive number.")
  }
  if (!is.numeric(ws) || ws <= 0) {
    warning("'ws' should be a positive number.")
  }

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
