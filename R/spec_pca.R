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
                     scale = T,
                     center = T){


  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
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

