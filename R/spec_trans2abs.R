#' Convert Spectral Data from Transmittance to Absorbance
#'
#' This function converts transmittance data to absorbance using the formula `A = 2 - log10(T)`, where `T` is the transmittance. It also filters out any infinite values resulting from the transformation, while preserving the wavelength column.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the converted absorbance data, containing the wavelength column and the absorbance numeric columns.
#'
#' @importFrom dplyr select where filter_all all_vars %>% if_all
#' @importFrom tidyselect everything
#' @importFrom recipes recipe step_mutate_at prep bake all_numeric_predictors
#' @importFrom stats as.formula
#'
#' @export

spec_trans2abs <- function(.data, wn_col = NULL) {

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

  numeric_cols <- dplyr::select(.data, -dplyr::all_of(wn_col)) %>%
    dplyr::select(where(is.numeric)) %>%
    names()

  if (length(numeric_cols) == 0) {
    warning("No numeric columns to convert found besides the wn_col column.")
    return(dplyr::select(.data, dplyr::all_of(wn_col)))
  }

 check_neg <- .data %>%
    dplyr::select(dplyr::all_of(numeric_cols)) %>%
    dplyr::filter(dplyr::if_any(tidyselect::everything(), ~ . <= 0))

  if (nrow(check_neg) > 0) {
    warning("There are values less than or equal to zero in the transmittance columns, which will result in -Inf or NaN after the logarithmic transformation.")
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))

  out <- tryCatch({
    recipes::recipe(formula = fmla, data = .data) %>%
      recipes::step_mutate_at(recipes::all_numeric_predictors(),
                              fn = \(x) 2 - log10(x)) %>%
      recipes::prep() %>%
      recipes::bake(NULL) %>%
      dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
      dplyr::filter(dplyr::if_all(tidyselect::everything(), ~ !is.infinite(.)))
  },
  error = function(e) {
    stop("Error during transmittance to absorbance conversion: ", e$message)
  })

  return(out)
}
