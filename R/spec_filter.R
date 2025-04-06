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

  wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                 ifnotfound = NULL)
  if (is.null(wn_col)) {
    stop("wn_col not specified and no pattern defined with set_spec_wn()")
  }

  wn_vals <- .data[[wn_col]]
  wn_range <- range(wn_vals, na.rm = TRUE)

  if (!is.null(wn_min) && wn_min < wn_range[1]) {
    stop(glue::glue("wn_min ({wn_min}) is below the minimum available wavenumber ({wn_range[1]})."))
  }

  if (!is.null(wn_max) && wn_max > wn_range[2]) {
    stop(glue::glue("wn_max ({wn_max}) is above the maximum available wavenumber ({wn_range[2]})."))
  }

  .data <- .data %>%
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(wn_col), ~ {
        (.x >= wn_min | is.null(wn_min)) &
          (.x <= wn_max | is.null(wn_max))
      })
    )

  return(.data)
}
