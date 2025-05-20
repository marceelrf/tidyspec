#' Filter spectral data by wavenumber range
#'
#' This function filters the spectral dataset based on a specified wavenumber (`wn`) range.
#' It requires the wavenumber column to be previously set using [set_spec_wn()].
#' If `wn_min` and/or `wn_max` are provided, the data will be filtered accordingly.
#' If neither is provided, the original dataset is returned unchanged.
#'
#' @param .data A data frame containing spectral data.
#' @param wn_min Optional numeric value. Minimum wavenumber value to keep.
#' @param wn_max Optional numeric value. Maximum wavenumber value to keep.
#'
#' @return A filtered data frame based on the wavenumber column.
#'
#' @importFrom dplyr filter all_of if_all %>%
#' @examples
#' set_spec_wn("Wavenumber")
#' spec_filter(CoHAspec, wn_min = 500, wn_max = 1800)
#'
#' @export

spec_filter <- function(.data, wn_min = NULL, wn_max = NULL) {

  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
  }

  wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
  if (is.null(wn_col)) {
    stop("The 'wn_col' argument was not specified and no default was defined with set_spec_wn().")
  }

  if (!wn_col %in% names(.data)) {
    stop(glue::glue("Column '{wn_col}' was not found in the provided data."))
  }

  if (!is.numeric(.data[[wn_col]])) {
    stop(glue::glue("Column '{wn_col}' must contain numeric values."))
  }

  wn_vals <- .data[[wn_col]]
  wn_range <- range(wn_vals, na.rm = TRUE)

  if (!is.null(wn_min) && !is.numeric(wn_min)) {
    stop("The 'wn_min' argument must be a numeric value or NULL.")
  }

  if (!is.null(wn_max) && !is.numeric(wn_max)) {
    stop("The 'wn_max' argument must be a numeric value or NULL.")
  }

  if (!is.null(wn_min) && wn_min < wn_range[1]) {
    warning(glue::glue("wn_min ({wn_min}) is below the available minimum ({wn_range[1]}). No data will be returned if out of range."))
  }

  if (!is.null(wn_max) && wn_max > wn_range[2]) {
    warning(glue::glue("wn_max ({wn_max}) is above the available maximum ({wn_range[2]}). No data will be returned if out of range."))
  }

  if (!is.null(wn_min) && !is.null(wn_max) && wn_min > wn_max) {
    stop("The value of 'wn_min' cannot be greater than 'wn_max'.")
  }

  if (is.null(wn_min) && is.null(wn_max)) {
    warning("No value for 'wn_min' or 'wn_max' was provided. The data will be returned without filtering.")
    return(.data)
  }

  .data <- .data %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(wn_col), ~ {
        (.x >= wn_min | is.null(wn_min)) & (.x <= wn_max | is.null(wn_max))
      })
    )

  return(.data)
}
