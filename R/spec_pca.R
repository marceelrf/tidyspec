#' Perform Principal Component Analysis (PCA) on Spectral Data
#'
#' This function computes a Principal Component Analysis (PCA) on spectral data,
#' excluding the wavenumber column from the analysis.
#'
#' @param .data A data frame containing spectral data, with one column representing
#'   wavenumbers and the remaining columns containing spectral intensity values.
#' @param wn_col A string specifying the name of the column that contains the
#'   wavenumber values. If NULL, the function attempts to retrieve the default
#'   wavenumber column set by `set_spec_wn()`.
#' @param scale A logical value indicating whether the spectral data should be
#'   scaled (default is TRUE).
#' @param center A logical value indicating whether the spectral data should be
#'   centered (default is TRUE).
#'
#' @return A `prcomp` object containing the PCA results, including principal
#'   components, standard deviations, and loadings.
#'
#' @importFrom stats prcomp
#'
#' @examples
#' \dontrun{
#' set_spec_wn("Wavenumber")
#' pca_result <- spec_pca(CoHAspec)
#' summary(pca_result)
#' }
#'
#' @export

spec_pca <- function(.data, wn_col = NULL,
                     scale = TRUE,
                     center = TRUE){


  if (!is.data.frame(.data)) {
    stop("The argument '.data' must be a data.frame or tibble.")
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

  spec_data <- dplyr::select(.data, -dplyr::all_of(wn_col))

  if (ncol(spec_data) == 0) {
    stop("There are no numeric columns to perform principal component analysis (PCA).")
  }

  if (!all(sapply(spec_data, is.numeric))) {
    stop("All spectral columns (except the wavenumber column) must be numeric.")
  }

  spec_matrix <- .data %>%
    dplyr::select(-dplyr::all_of(wn_col)) %>%
    as.matrix() %>%
    t()

  colnames(spec_matrix) <- dplyr::pull(.data,
                                dplyr::all_of(wn_col))

  if (!is.numeric(spec_matrix)) {
    stop("Spectral data must be numeric.")
  }

  pca_result <- prcomp(spec_matrix, scale. = scale, center = center)

  return(pca_result)

}

