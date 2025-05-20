#' Convert Absorbance Data to Transmittance
#'
#' This function converts absorbance data to transmittance using the formula \eqn{T = 10^{(2 - A)}}, where \eqn{A} is the absorbance and \eqn{T} is the transmittance.
#'
#' @param .data A `data.frame` or `tibble` containing spectral data in absorbance.
#' @param wn_col A character string specifying the column name for the wavelength data. Default is `"Wn"`.
#'
#' @return A `tibble` with the converted transmittance data, containing the wavelength column and the numeric transmittance columns. Any rows with infinite values are removed.
#'
#' @importFrom dplyr select where filter_all all_vars %>% if_all
#' @importFrom tidyselect everything
#' @importFrom recipes recipe step_mutate_at prep bake all_predictors
#' @importFrom stats as.formula
#' @export
spec_abs2trans <- function(.data, wn_col = NULL) {

  if (missing(.data)) {
    warning("Argument '.data' is missing. Please provide a data.frame or tibble with absorbance data.")
  }

  if (!inherits(.data, c("data.frame", "tbl", "tbl_df"))) {
    warning("Argument '.data' must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  if (!wn_col %in% colnames(.data)) {
    warning(paste0("Column '", wn_col, "' not found in the input data."))
  }

  num_cols <- setdiff(names(.data)[sapply(.data, is.numeric)], wn_col)
  if (length(num_cols) == 0) {
    warning("No numeric columns found to convert to transmittance.")
  }

  if (any(!sapply(.data[num_cols], is.numeric))) {
    warning("Some absorbance columns contain non-numeric values. These columns will be ignored.")
  }
  if (any(is.na(.data[num_cols]))) {
    warning("Some absorbance values are NA. Resulting transmittance values may also be NA.")
  }

  fmla <- stats::as.formula(paste(wn_col, " ~ .", sep = ""))
  result <- recipes::recipe(formula = fmla, data = .data) %>%
    recipes::step_mutate_at(recipes::all_predictors(),
                            fn = \(x) 10^(2 - x)) %>%
    recipes::prep() %>%
    recipes::bake(NULL) %>%
    dplyr::select({{wn_col}}, dplyr::where(is.numeric)) %>%
    dplyr::filter(dplyr::if_all(tidyselect::everything(), ~ !is.infinite(.)))

  n_removed <- nrow(.data) - nrow(result)

  if (n_removed > 0) {
    warning(paste(n_removed, "rows removed due to infinite values after conversion."))
  }

  result
}
