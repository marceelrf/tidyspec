#' Apply Savitzky-Golay Smoothing to Spectral Data
#'
#' This function applies Savitzky-Golay smoothing to numeric spectral data using a specified window size, polynomial order, and differentiation degree, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param window A numeric value specifying the window size for the Savitzky-Golay smoothing. Default is 15.
#' @param forder A numeric value specifying the polynomial order for smoothing. Default is 4.
#' @param degree A numeric value specifying the degree of differentiation. Default is 0 (no differentiation).
#'
#' @return A `tibble` with the smoothed spectral data, containing the wavelength column and the smoothed numeric columns.
#'
#' @importFrom dplyr select where %>%
#' @importFrom recipes recipe step_mutate_at prep bake all_numeric_predictors
#' @importFrom stats as.formula
#' @importFrom signal sgolayfilt
#'
#' @export

spec_smooth_sga <- function(.data, wn_col = NULL, window = 15, forder = 4, degree = 0) {

  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no default defined with set_spec_wn().")
    } else {
      warning(sprintf("wn_col not provided. Using defined default: '%s'.", wn_col))
    }
  }

  if (!(wn_col %in% names(.data))) {
    stop(sprintf("Column '%s' was not found in the dataset.", wn_col))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(sprintf("Column '%s' must be numeric.", wn_col))
  }

  if (!is.numeric(window) || length(window) != 1 || window <= 0 || window %% 2 == 0) {
    warning("Argument 'window' must be a positive odd number. Adjusting to default value 15.")
    window <- 15
  }

  if (!is.numeric(forder) || length(forder) != 1 || forder < 0) {
    warning("Argument 'forder' must be a non-negative integer. Adjusting to default value 4.")
    forder <- 4
  } else {
    forder <- floor(forder)
  }

  if (!is.numeric(degree) || length(degree) != 1 || degree < 0) {
    warning("Argument 'degree' must be a non-negative integer. Adjusting to default value 0.")
    degree <- 0
  } else {
    degree <- floor(degree)
  }

  if (window <= forder) {
    warning("Window size 'window' must be greater than polynomial degree 'forder'. Adjusting window to forder + 2 (odd).")
    window <- forder + 2
    if (window %% 2 == 0) window <- window + 1
  }

  numeric_cols <- dplyr::select(.data, -dplyr::all_of(wn_col)) %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  if (length(numeric_cols) == 0) {
    warning("No numeric columns found to smooth besides the wn_col column.")
    return(dplyr::select(.data, dplyr::all_of(wn_col)))
  }


  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  out <- tryCatch(
    recipes::recipe(formula = fmla, data = .data) %>%
      recipes::step_mutate_at(recipes::all_numeric_predictors(),
                              fn = function(x) signal::sgolayfilt(x = x, p = forder, n = window, m = degree)) %>%
      recipes::prep() %>%
      recipes::bake(NULL) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric)),
    error = function(e) {
      stop("Error applying Savitzky-Golay smoothing: ", e$message)
    }
  )

  return(out)
}
