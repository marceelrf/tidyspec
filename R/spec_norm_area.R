#' Normalize Spectral Data by Area Under the Curve
#'
#' This function normalizes the numeric spectral data in each column so that the area under the curve equals a specified value (default is 1), while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. If NULL, uses the default set by `set_spec_wn()`.
#' @param target_area A numeric value specifying the target area under the curve. Default is 1.
#'
#' @return A `tibble` with the area-normalized spectral data, containing the wavelength column and the normalized numeric columns.
#'
#' @importFrom dplyr select where %>% mutate across
#' @importFrom stats approx
#'
#' @export
spec_norm_area <- function(.data, wn_col = NULL, target_area = 1) {
  # Input validation
  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (!is.numeric(target_area) || length(target_area) != 1 || target_area <= 0) {
    stop("The 'target_area' argument must be a positive numeric value.")
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

  # Calculate area normalization
  wn_values <- .data[[wn_col]]

  # Check if wavenumbers are sorted
  if (is.unsorted(wn_values)) {
    warning("Wavenumber values are not sorted. Results may be unexpected.")
  }

  # Normalize each numeric column by its area under the curve
  normalized_data <- .data %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric) & !{{wn_col}},
        ~ {
          # Calculate area using trapezoidal rule
          current_area <- pracma::trapz(wn_values, .x)
          if (abs(current_area) < .Machine$double.eps) {
            warning("Area under curve is approximately zero for one or more columns. Returning original values.")
            return(.x)
          }
          # Normalize to target area
          .x * (target_area / current_area)
        }
      )
    )

  return(normalized_data)
}
