#' Check the Currently Set Wavenumber Column
#'
#' This function checks which column has been set as the default wavenumber column.
#' If no column has been set, it prints a message prompting the user to define one.
#'
#' @param show_val Logical. If `TRUE`, the function returns the name of the current wavenumber column. Default is `FALSE`.
#'
#' @return If `show_val = TRUE`, returns the name of the wavenumber column (character). Otherwise, it only prints a message.
#'
#' @importFrom crayon red bold
#' @importFrom glue glue
#'
#' @examples
#' set_spec_wn("Wavenumber")  # Set the wavenumber column
#' check_wn_col()             # Check which column is set
#' check_wn_col(show_val = TRUE)  # Check and return the column name
#'
#' @export
check_wn_col <- function(show_val = FALSE) {
  wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)

  red_bold <- crayon::red $ bold

  if (!exists("tidyspec_env")) {
    warning("The 'tidyspec_env' environment is not available. Please check if it was created correctly.")
    return(invisible(NULL))
  }

  if (is.null(wn_col)) {
    message(glue::glue_col("No wavenumber column has been set. Use {red_bold('set_spec_wn()')} to define it."))
    return(invisible(NULL))
  }

  if (!is.character(wn_col)) {
    warning("The value stored in .wn_col_default is not a string. This can cause failures in dependent functions.")
    return(invisible(NULL))
  }

  if (length(wn_col) != 1) {
    warning("The value of .wn_col_default must contain only one column name. Please check the definition.")
    return(invisible(NULL))
  }

  if (nchar(wn_col) == 0 || wn_col == "") {
    warning("The wavenumber column name is empty. Use set_spec_wn() with a valid name.")
    return(invisible(NULL))
  }

  message(glue::glue_col("The current wavenumber column is: {red_bold(wn_col)}"))

  if (show_val) {
    return(wn_col)
  }
}
