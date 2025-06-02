#' Apply Smoothing to Spectral Data Using a Moving Average
#'
#' This function applies a moving average smoothing to numeric spectral data using a specified window size and polynomial degree, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param window A numeric value specifying the window size for the moving average smoothing. Default is 15.
#' @param degree A numeric value specifying the degree of the polynomial for smoothing. Default is 2.
#'
#' @return A `tibble` with the smoothed spectral data, containing the wavelength column and the smoothed numeric columns.
#'
#' @importFrom dplyr select where %>%
#' @importFrom recipes recipe prep bake all_numeric_predictors
#' @importFrom stats as.formula
#' @importFrom timetk step_smooth
#'
#' @export
spec_smooth_avg <- function(.data, wn_col = NULL, window = 15, degree = 2) {

  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no default defined with set_spec_wn().")
    } else {
      warn_missing_param_once("wn_col", wn_col)
    }
  }

  if (!(wn_col %in% names(.data))) {
    stop(sprintf("Column '%s' was not found in the dataset.", wn_col))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(sprintf("Column '%s' must be numeric.", wn_col))
  }

  if (!is.numeric(window) || length(window) != 1 || window <= 0) {
    warning("Argument 'window' must be a positive number. Using default value 15.")
    window <- 15
  }

  if (!is.numeric(degree) || length(degree) != 1 || degree < 0) {
    warning("Argument 'degree' must be a number >= 0. Using default value 2.")
    degree <- 2
  }

  numeric_cols <- dplyr::select(.data, -dplyr::all_of(wn_col)) %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  if (length(numeric_cols) == 0) {
    warning("No numeric columns found to smooth besides the wn_col column.")
    return(dplyr::select(.data, dplyr::all_of(wn_col)))
  }


  fmla <- stats::as.formula(paste(wn_col," ~ .", sep = ""))

  out <- tryCatch(
    recipes::recipe(formula = fmla, data = .data) %>%
      timetk::step_smooth(recipes::all_numeric_predictors(),
                          period = window,
                          degree = degree) %>%
      recipes::prep() %>%
      recipes::bake(NULL) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric)),
    error = function(e) {
      stop("Error applying smoothing: ", e$message)
    }
  )

  return(out)
}
