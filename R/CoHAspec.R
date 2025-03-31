#' CoHAspec Dataset
#'
#' A dataset containing spectral absorbance measurements for different concentrations of CoHA.
#'
#' @format A data frame with 6 rows and 5 columns:
#' \describe{
#'   \item{Wavenumber}{Numeric. The spectral wavenumber (cm⁻¹).}
#'   \item{CoHA01}{Numeric. Absorbance values for CoHA at 1 mM Cobalt concentration.}
#'   \item{CoHA025}{Numeric. Absorbance values for CoHA at 2.5 mM Cobalt concentration.}
#'   \item{CoHA05}{Numeric. Absorbance values for CoHA at 5 mM Cobalt concentration.}
#'   \item{CoHA100}{Numeric. Absorbance values for CoHA at 10 mM Cobalt concentration.}
#' }
#'
#' @source de Almeida GS, Ferreira MR, da Costa Fernandes CJ, et al. Development of cobalt (Co)-doped monetites for bone regeneration. J Biomed Mater Res. 2024; 112(1):e35319. doi:10.1002/jbm.b.35319
#'
#' @examples
#' data(CoHAspec)
#' head(CoHAspec)
"CoHAspec"
