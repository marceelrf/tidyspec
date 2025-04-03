#' Compute Wavenumber Contributions to Principal Components
#'
#' This function calculates the contribution of each wavenumber to the principal components (PCs)
#' in a PCA result. Contributions are computed as the squared loadings multiplied by 100.
#'
#' @param PCA An object of class `prcomp`, containing the results of a principal component analysis.
#'
#' @return A tibble containing the wavenumber column and the percentage contribution of each
#' wavenumber to each principal component.
#'
#' @details The function extracts the PCA loadings (rotation matrix) and computes the squared
#' values of each loading, scaled to percentage values. This helps interpret the importance of
#' each wavenumber in defining the principal components.
#'
#' @examples
#' \dontrun{
#' pca_result <- spec_pca(spectral_data)
#' wn_contrib <- spec_pca_wn_contrib(pca_result)
#' print(wn_contrib)
#' }
#'
#' @export
spec_pca_wn_contrib <- function(PCA){

    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }

  res <- NULL

  res[[wn_col]] <- as.numeric(rownames(PCA$rotation))

  res <- dplyr::bind_cols(res,PCA$rotation) %>%
    dplyr::mutate(dplyr::across(-dplyr::all_of(wn_col), \(x) 100*x^2))

  return(res)
}
