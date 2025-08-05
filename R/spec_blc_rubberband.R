#' Apply Rubberband Baseline Correction to Spectral Data
#'
#' This function applies a rubberband baseline correction to spectral data within a specified wavelength range.
#' The rubberband method fits a baseline by connecting local minima points in the spectrum.
#' It allows for correction of either absorbance or transmittance data.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param wn_min A numeric value specifying the minimum wavelength to consider for the baseline correction.
#' @param wn_max A numeric value specifying the maximum wavelength to consider for the baseline correction.
#' @param segment_length A numeric value specifying the length of segments for finding local minima. Default is 50.
#' @param smooth_baseline A logical value indicating whether to smooth the baseline using spline interpolation. Default is TRUE.
#' @param is_abs A logical value indicating whether the data is already in absorbance. If `TRUE`, absorbance is used directly; if `FALSE`, the data is converted to absorbance before applying the baseline correction.
#'
#' @return A `tibble` with the baseline-corrected spectral data, containing the wavelength column and the corrected numeric columns.
#'
#' @importFrom dplyr select mutate where %>%
#' @importFrom purrr map_dfc
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
#' @importFrom stats approx spline
#'
#' @references
#' Rubberband baseline correction connects local minima to estimate baseline.
#'
#' @export
spec_blc_rubberband <- function(.data,
                                wn_col = NULL,
                                wn_min = NULL,
                                wn_max = NULL,
                                segment_length = 50,
                                smooth_baseline = TRUE,
                                is_abs = TRUE) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
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

  if (!is.numeric(segment_length) || segment_length <= 0) {
    warning("'segment_length' should be a positive number.")
  }

  if (!is.logical(smooth_baseline) || length(smooth_baseline) != 1) {
    stop("The 'smooth_baseline' argument must be TRUE or FALSE.")
  }

  # Helper function to apply rubberband correction
  rubberband_correction <- function(y, x = NULL, segment_length = 50, smooth_baseline = TRUE) {
    if (is.null(x)) {
      x <- seq_along(y)
    }

    if (all(is.na(y))) {
      return(list(corrected = y, baseline = rep(NA, length(y))))
    }

    n <- length(y)
    if (n < 3) {
      return(list(corrected = y, baseline = rep(mean(y, na.rm = TRUE), n)))
    }

    # Find local minima in segments
    n_segments <- max(1, floor(n / segment_length))
    segment_size <- floor(n / n_segments)

    minima_indices <- c()
    minima_values <- c()

    # Always include first and last points
    minima_indices <- c(1)
    minima_values <- c(y[1])

    # Find local minima in each segment
    for (i in 1:n_segments) {
      start_idx <- (i - 1) * segment_size + 1
      end_idx <- min(i * segment_size, n)

      if (start_idx < end_idx) {
        segment_y <- y[start_idx:end_idx]
        local_min_idx <- which.min(segment_y)
        global_min_idx <- start_idx + local_min_idx - 1

        # Avoid duplicate indices
        if (!global_min_idx %in% minima_indices) {
          minima_indices <- c(minima_indices, global_min_idx)
          minima_values <- c(minima_values, y[global_min_idx])
        }
      }
    }

    # Always include last point if not already included
    if (!n %in% minima_indices) {
      minima_indices <- c(minima_indices, n)
      minima_values <- c(minima_values, y[n])
    }

    # Sort by index
    order_idx <- order(minima_indices)
    minima_indices <- minima_indices[order_idx]
    minima_values <- minima_values[order_idx]

    # Create baseline by interpolating between minima
    if (smooth_baseline && length(minima_indices) > 3) {
      # Use spline interpolation for smooth baseline
      baseline <- stats::spline(x = minima_indices, y = minima_values,
                                xout = 1:n, method = "natural")$y
    } else {
      # Use linear interpolation
      baseline <- stats::approx(x = minima_indices, y = minima_values,
                                xout = 1:n, method = "linear",
                                rule = 2)$y
    }

    # Correct the spectrum
    corrected <- y - baseline

    return(list(corrected = corrected, baseline = baseline))
  }

  # Helper function to apply rubberband to each spectrum
  apply_rubberband <- function(spectrum_data) {
    # Apply rubberband correction to each numeric column
    corrected_data <- purrr::map_dfc(spectrum_data[num_cols], function(col) {
      if (all(is.na(col))) {
        return(col)  # Return as-is if all NA
      }

      # Apply rubberband correction
      result <- rubberband_correction(col,
                                      segment_length = segment_length,
                                      smooth_baseline = smooth_baseline)
      return(result$corrected)
    })

    # Add wavelength column back
    corrected_data[[wn_col]] <- spectrum_data[[wn_col]]

    # Reorder columns to match original order
    corrected_data[c(wn_col, num_cols)]
  }

  if (is_abs) {
    apply_rubberband(mat)
  } else {
    # Convert to absorbance first, then apply correction
    abs_data <- spec_trans2abs(mat, wn_col = !!rlang::sym(wn_col))
    apply_rubberband(abs_data)
  }
}
