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
#' @importFrom purrr map_dfc
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
#'
#' @references
#' Baseline estimation performed using a custom rolling ball implementation.
#'
#' @export
spec_blc_rollingBall <- function(.data,
                                 wn_col = NULL,
                                 wn_min = NULL,
                                 wn_max = NULL,
                                 wm,
                                 ws = 0,
                                 is_abs = TRUE) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (missing(wm)) {
    stop("Argument 'wm' is required.")
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
    # Note: You might want to add your warn_missing_param_once function here
    # warn_missing_param_once("wn_min", wn_min)
  }

  if (is.null(wn_max)) {
    wn_max <- max(wn_values, na.rm = TRUE)
    # warn_missing_param_once("wn_max", wn_max)
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
  if (!is.numeric(ws) || ws < 0) {
    warning("'ws' should be a non-negative number.")
  }

  # Helper function to apply rolling ball to each spectrum
  apply_rolling_ball <- function(spectrum_data) {
    # Apply rolling ball correction to each numeric column
    corrected_data <- purrr::map_dfc(spectrum_data[num_cols], function(col) {
      if (all(is.na(col))) {
        return(col)  # Return as-is if all NA
      }

      # Apply rolling ball correction
      result <- rolling_ball(col, wm = wm, ws = ws)
      return(result$corrected)
    })

    # Add wavelength column back
    corrected_data[[wn_col]] <- spectrum_data[[wn_col]]

    # Reorder columns to match original order
    corrected_data[c(wn_col, num_cols)]
  }

  if (is_abs) {
    apply_rolling_ball(mat)
  } else {
    # Convert to absorbance first, then apply correction
    abs_data <- spec_trans2abs(mat, wn_col = !!rlang::sym(wn_col))
    apply_rolling_ball(abs_data)
  }
}
