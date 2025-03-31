#' Set the Default Wavenumber Column
#'
#' Defines a default wavenumber column name to be used by other functions in the package when `wn_col` is not explicitly provided.
#'
#' @param col_name A string specifying the name of the wavenumber column.
#'
#' @return Invisibly returns the assigned column name.
#' @export
#'
#' @seealso [spec_select()]
#'
#' @examples
#' set_spec_wn("Wavenumber")
set_spec_wn <- function(col_name) {
  assign(".wn_col_default", col_name, envir = tidyspec_env)
  invisible(col_name)
}
