#' Interpolate Spectral Data to Match Reference Wavenumbers (Left Join Style)
#'
#' This function interpolates spectral data to match the wavenumber grid of a reference dataset.
#' Works like a left join - keeps all wavenumbers from the reference data and interpolates
#' the spectral data to match those wavenumbers.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data to be interpolated.
#' @param reference A `data.frame` or `tibble` containing the reference wavenumber grid.
#' @param wn_col A character string specifying the column name for the wavenumber data.
#'   If NULL, uses the default set with set_spec_wn().
#' @param method A character string specifying the interpolation method.
#'   Options: "linear", "pchip", "spline", "akima". Default is "pchip".
#' @param extrapolate Logical. Should values be extrapolated outside the original range?
#'   Default is FALSE (returns NA for out-of-range values).
#' @param ... Additional arguments passed to the interpolation method.
#'
#' @return A `tibble` with interpolated spectral data using the reference wavenumber grid.
#'
#' @importFrom dplyr select mutate where %>%
#' @importFrom purrr map_dfc
#' @importFrom tibble as_tibble
#' @importFrom rlang :=
#'
#' @export
spec_interpolate_left <- function(.data,
                                  reference,
                                  wn_col = NULL,
                                  method = "pchip",
                                  extrapolate = FALSE,
                                  ...) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (!is.data.frame(reference)) {
    stop("The argument 'reference' must be a data.frame or tibble.")
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

  if (!wn_col %in% names(reference)) {
    stop(paste0("Column '", wn_col, "' was not found in 'reference'."))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(paste0("Column '", wn_col, "' must contain numeric values in .data."))
  }

  if (!is.numeric(reference[[wn_col]])) {
    stop(paste0("Column '", wn_col, "' must contain numeric values in reference."))
  }

  # Get target wavenumbers from reference
  target_wn <- reference[[wn_col]]

  # Identify numeric columns (spectra) in .data
  num_cols <- setdiff(names(.data), wn_col)
  if (length(num_cols) == 0) {
    stop("No numeric columns found besides 'wn_col' in .data.")
  }

  # Create result tibble starting with reference wavenumbers
  result <- tibble::tibble(!!wn_col := target_wn)

  # Interpolate each spectrum column
  for (col_name in num_cols) {
    result[[col_name]] <- interpolate_column(
      x = .data[[wn_col]],
      y = .data[[col_name]],
      xout = target_wn,
      method = method,
      extrapolate = extrapolate,
      ...
    )
  }

  return(result)
}

#' Interpolate Spectral Data to Include Both Grids (Right Join Style)
#'
#' This function interpolates reference data to match the wavenumber grid of the main dataset.
#' Works like a right join - keeps all wavenumbers from .data and interpolates reference
#' data to match those wavenumbers.
#'
#' @param .data A `data.frame` or `tibble` containing the main spectral data.
#' @param reference A `data.frame` or `tibble` containing spectral data to be interpolated.
#' @param wn_col A character string specifying the column name for the wavenumber data.
#' @param method A character string specifying the interpolation method.
#' @param extrapolate Logical. Should values be extrapolated outside the original range?
#' @param suffix Character vector of length 2 specifying suffixes for overlapping column names.
#' @param ... Additional arguments passed to the interpolation method.
#'
#' @return A `tibble` combining .data with interpolated reference data.
#'
#' @export
spec_interpolate_right <- function(.data,
                                   reference,
                                   wn_col = NULL,
                                   method = "pchip",
                                   extrapolate = FALSE,
                                   suffix = c(".x", ".y"),
                                   ...) {

  # Similar validation as spec_interpolate_left...
  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    }
  }

  # Interpolate reference to match .data wavenumbers
  interpolated_ref <- spec_interpolate_left(
    reference, .data, wn_col = wn_col, method = method,
    extrapolate = extrapolate, ...
  )

  # Combine with original data, handling name conflicts
  ref_cols <- setdiff(names(interpolated_ref), wn_col)
  data_cols <- setdiff(names(.data), wn_col)

  # Handle overlapping column names
  overlapping <- intersect(ref_cols, data_cols)
  if (length(overlapping) > 0) {
    # Rename overlapping columns
    for (col in overlapping) {
      names(.data)[names(.data) == col] <- paste0(col, suffix[1])
      names(interpolated_ref)[names(interpolated_ref) == col] <- paste0(col, suffix[2])
    }
  }

  # Combine datasets
  result <- .data
  for (col in setdiff(names(interpolated_ref), wn_col)) {
    result[[col]] <- interpolated_ref[[col]]
  }

  return(result)
}

#' Interpolate to Regular Grid
#'
#' Creates a regular wavenumber grid and interpolates spectral data to match it.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param resolution Numeric. The step size for the regular grid.
#' @param wn_min,wn_max Numeric. Range for the regular grid. If NULL, uses data range.
#' @param wn_col A character string specifying the column name for the wavenumber data.
#' @param method A character string specifying the interpolation method.
#' @param ... Additional arguments passed to the interpolation method.
#'
#' @return A `tibble` with spectral data interpolated to a regular grid.
#'
#' @importFrom pracma pchip
#' @importFrom akima aspline
#'
#' @export
spec_interpolate_regular <- function(.data,
                                     resolution = 1,
                                     wn_min = NULL,
                                     wn_max = NULL,
                                     wn_col = NULL,
                                     method = "pchip",
                                     ...) {

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    }
  }

  wn_values <- .data[[wn_col]]

  if (is.null(wn_min)) {
    wn_min <- min(wn_values, na.rm = TRUE)
  }

  if (is.null(wn_max)) {
    wn_max <- max(wn_values, na.rm = TRUE)
  }

  # Create regular grid
  target_wn <- seq(wn_min, wn_max, by = resolution)

  # Create reference tibble
  reference <- tibble::tibble(!!wn_col := target_wn)

  # Use spec_interpolate_left
  spec_interpolate_left(.data, reference, wn_col = wn_col, method = method, ...)
}

# Helper function for different interpolation methods
interpolate_column <- function(x, y, xout, method = "pchip", extrapolate = FALSE, ...) {

  # Remove NA values
  valid_idx <- !is.na(x) & !is.na(y)
  x_clean <- x[valid_idx]
  y_clean <- y[valid_idx]

  if (length(x_clean) < 2) {
    warning("Not enough valid data points for interpolation. Returning NA.")
    return(rep(NA_real_, length(xout)))
  }

  # Handle extrapolation
  if (!extrapolate) {
    out_of_range <- xout < min(x_clean) | xout > max(x_clean)
  }

  result <- switch(method,
                   "linear" = {
                     stats::approx(x_clean, y_clean, xout, rule = if(extrapolate) 2 else 1)$y
                   },
                   "pchip" = {
                     if (!requireNamespace("pracma", quietly = TRUE)) {
                       stop("Package 'pracma' is required for PCHIP interpolation.")
                     }
                     pracma::pchip(x_clean, y_clean, xout)
                   },
                   "spline" = {
                     stats::spline(x_clean, y_clean, xout = xout, ...)$y
                   },
                   "akima" = {
                     if (!requireNamespace("akima", quietly = TRUE)) {
                       stop("Package 'akima' is required for Akima interpolation.")
                     }
                     akima::aspline(x_clean, y_clean, xout)$y
                   },
                   stop("Unknown interpolation method: ", method)
  )

  # Apply extrapolation rule
  if (!extrapolate && exists("out_of_range")) {
    result[out_of_range] <- NA_real_
  }

  return(result)
}
