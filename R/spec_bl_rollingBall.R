#' Apply Rolling Ball Baseline Correction to Spectral Data
#'
#' This function applies a rolling ball baseline correction to spectral data within a specified wavelength range.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#' @param wn_min A numeric value specifying the minimum wavelength to consider for the baseline correction.
#' @param wn_max A numeric value specifying the maximum wavelength to consider for the baseline correction.
#' @param wm A numeric value for the window size of the rolling ball algorithm.
#' @param ws A numeric value for the smoothing factor of the rolling ball algorithm.
#' @param is_abs A logical value indicating whether the data is already in absorbance. If `TRUE`, absorbance is used directly; if `FALSE`, the data is converted to absorbance before applying the baseline correction.
#'
#' @return A `tibble` with the corrected spectral data, containing the wavelength column and the corrected numeric columns.
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
spec_bl_rollingBall <- function(.data,
                                wn_col = NULL,
                                wn_min = NULL,
                                wn_max = NULL,
                                wm,
                                ws,
                                is_abs = TRUE) {
  if (missing(wm) || missing(ws)) {
    stop("Arguments 'wm' and 'ws' are required.")
  }

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
    } else if (!wn_col %in% colnames(.data)) {
      stop("The column defined in '.wn_col_default' is not present in the dataset.")
    }
  } else {
    if (!wn_col %in% colnames(.data)) {
      stop(glue::glue("The column '{wn_col}' is not present in the dataset."))
    }
  }

  if (!is.numeric(.data[[wn_col]])) {
    warning(glue::glue("The column '{wn_col}' is not numeric. Baseline correction may fail."))
  }

  wn_values <- .data[[wn_col]]

  if (is.null(wn_min)) {
    wn_min <- min(wn_values, na.rm = TRUE)
    warning("wn_min was not specified. Using the minimum value present in 'wn_col'.")
  }
  if (is.null(wn_max)) {
    wn_max <- max(wn_values, na.rm = TRUE)
    warning("wn_max was not specified. Using the maximum value present in 'wn_col'.")
  }

  if (wn_min >= wn_max) {
    stop("wn_min must be less than wn_max.")
  }

  in_range <- .data[[wn_col]] >= wn_min & .data[[wn_col]] <= wn_max
  if (!any(in_range)) {
    stop("No data found within the specified wn_min to wn_max range.")
  }

  mat <- .data[.data[[wn_col]] >= wn_min & .data[[wn_col]] <= wn_max, ]

  numeric_cols <- sapply(mat, is.numeric)
  numeric_cols[[wn_col]] <- FALSE
  if (sum(numeric_cols) == 0) {
    stop("No numeric columns found (except the wavenumber column) to apply correction.")
  }

  if (is_abs && any(mat[numeric_cols] < 0, na.rm = TRUE)) {
    warning("Negative values detected in absorbance data. Please verify data integrity.")
  }

  if (is_abs) {
    mat %>%
      dplyr::select(-{{wn_col}}) %>%
      t() %>%
      baseline::baseline(method = "rollingBall", wm = wm, ws = ws) %>%
      purrr::pluck("baseline") %>%
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
      purrr::pluck("baseline") %>%
      t() %>%
      tibble::as_tibble() %>%
      dplyr::mutate({{wn_col}} := mat[[wn_col]]) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric))
  }
}
