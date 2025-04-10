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
#'
#' @examples
#' set_spec_wn("Wavenumber")  # Set the wavenumber column
#' check_wn_col()             # Check which column is set
#' check_wn_col(show_val = TRUE)  # Check and return the column name
#'
#' @export
check_wn_col <- function(show_val = FALSE) {
  wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)

  red_bold <- red $ bold

  if (is.null(wn_col)) {
    message(glue::glue_col("No wavenumber column has been set. Use {red_bold {set_spec_wn()}} to define it."))
  }


  message(glue::glue_col("The current wavenumber column is: {red_bold {wn_col}}"))

  if (show_val) {
    return(wn_col)
  }
}

