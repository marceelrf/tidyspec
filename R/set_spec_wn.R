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
  # 1. Check if 'col_name' is missing
  if (missing(col_name)) {
    stop("Argument 'col_name' is missing with no default.", call. = FALSE)
  }

  # 2. Check if 'col_name' is a character string
  if (!is.character(col_name)) {
    stop("Argument 'col_name' should be a character string (e.g., 'Wavenumber').", call. = FALSE)
  }

  # 3. Check if 'col_name' has length 1
  if (length(col_name) != 1) {
    stop("Argument 'col_name' should be a single string (length 1).", call. = FALSE)
  }

  # 4. Check if 'col_name' is not an empty string
  if (nchar(col_name) == 0) {
    stop("Argument 'col_name' should not be an empty string.", call. = FALSE)
  }

  # 5. Check if the environment 'tidyspec_env' exists
  if (!exists("tidyspec_env", inherits = TRUE)) {
    warning("Environment 'tidyspec_env' does not exist. Creating it now and assigning the variable there.")
    tidyspec_env <<- new.env(parent = emptyenv())
  }

  assign(".wn_col_default", col_name, envir = tidyspec_env)
  message(glue::glue("Successfully set '{col_name}' as the default wavenumber column."))

  invisible(col_name)
}
