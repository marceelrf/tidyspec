#' Normalize Spectral Data by Specific Wavenumber Value
#'
#' This function normalizes the numeric spectral data in each column so that the intensity at a specified wavenumber equals a target value (default is 1), while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. If NULL, uses the default set by `set_spec_wn()`.
#' @param target_wn A numeric value specifying the wavenumber to use as reference for normalization.
#' @param target_value A numeric value specifying the target intensity value at the reference wavenumber. Default is 1.
#' @param method A character string specifying the interpolation method when the exact wavenumber is not found. Options are "linear" (default) or "nearest".
#'
#' @return A `tibble` with the wavenumber-normalized spectral data, containing the wavelength column and the normalized numeric columns.
#'
#' @importFrom dplyr select where %>% mutate across
#' @importFrom stats approx
#'
#' @export
spec_norm_by_wn <- function(.data, wn_col = NULL, target_wn, target_value = 1, method = "linear") {
  # Input validation
  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (missing(target_wn)) {
    stop("The 'target_wn' argument must be specified.")
  }

  if (!is.numeric(target_wn) || length(target_wn) != 1) {
    stop("The 'target_wn' argument must be a single numeric value.")
  }

  if (!is.numeric(target_value) || length(target_value) != 1) {
    stop("The 'target_value' argument must be a single numeric value.")
  }

  if (!method %in% c("linear", "nearest")) {
    stop("The 'method' argument must be either 'linear' or 'nearest'.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    }
  }

  if (!wn_col %in% names(.data)) {
    stop(glue::glue("Column '{wn_col}' was not found in the provided data."))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(glue::glue("Column '{wn_col}' must contain numeric values."))
  }

  # Get numeric columns (excluding wavenumber column)
  numeric_cols <- dplyr::select(.data, -{{wn_col}}, dplyr::where(is.numeric))

  if (ncol(numeric_cols) == 0) {
    warning("No numeric columns found for normalization. Only the wavenumber column will be returned.")
    return(dplyr::select(.data, {{wn_col}}))
  }

  # Get wavenumber values
  wn_values <- .data[[wn_col]]

  # Check if target_wn is within range
  if (target_wn < min(wn_values) || target_wn > max(wn_values)) {
    stop(glue::glue("The target wavenumber {target_wn} is outside the range of available wavenumbers ({min(wn_values)} - {max(wn_values)})."))
  }

  # Normalize each numeric column by the intensity at target_wn
  normalized_data <- .data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric) & !{{wn_col}},
        ~ {
          # Find intensity at target_wn
          if (target_wn %in% wn_values) {
            # Exact match found
            ref_intensity <- .x[wn_values == target_wn][1]
          } else {
            # Interpolate to find intensity at target_wn
            if (method == "linear") {
              ref_intensity <- stats::approx(wn_values, .x, xout = target_wn, method = "linear")$y
            } else { # nearest
              nearest_idx <- which.min(abs(wn_values - target_wn))
              ref_intensity <- .x[nearest_idx]
            }
          }

          if (is.na(ref_intensity) || abs(ref_intensity) < .Machine$double.eps) {
            warning(glue::glue("Reference intensity at wavenumber {target_wn} is zero or NA for one or more columns. Returning original values."))
            return(.x)
          }

          # Normalize to target value
          .x * (target_value / ref_intensity)
        }
      )
    )

  return(normalized_data)
}
