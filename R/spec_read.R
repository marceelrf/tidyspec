#' Read Spectral Data from Various File Formats
#'
#' This function reads spectral data from a file, automatically detecting the format
#' and using either `readr` or `readxl` functions. It also sets the specified
#' wavenumber column as the default using `set_spec_wn()`.
#'
#' @param file Path to the file to be read.
#' @param wn_col Character. Name of the column containing wavenumber values. This column
#'   will be set as the default wavenumber column.
#' @param ... Additional arguments passed to `readr::read_delim()`, `readr::read_csv()`,
#'   or `readxl::read_excel()`, depending on the file format.
#'
#' @return A tibble containing the spectral data.
#'
#' @importFrom readr read_csv read_csv2 read_tsv
#' @importFrom readxl read_xls read_xlsx
#'
#' @details The function automatically determines the file format based on the extension:
#'   - `.txt`, `.csv`, `.tsv`, and `.csv2` are read using `readr` functions.
#'   - `.xls` and `.xlsx` are read using `readxl::read_excel()`.
#'
#'   If the specified `wn_col` does not exist in the data, an error is returned.
#'
#' @examples
#' \dontrun{
#'   df <- spec_read("spectra.csv", wn_col = "Wavenumber")
#'   check_wn_col() # Verify that the wavenumber column is set
#' }
#'
#' @export
spec_read <- function(file, wn_col, ...) {
  # Detect file extension
  ext <- tools::file_ext(file)

  # Read the file based on the extension
  if (ext %in% c("txt", "tsv")) {
    df <- readr::read_tsv(file, ...)
  } else if (ext %in% c("csv")) {
    df <- readr::read_csv(file, ...)
  } else if (ext %in% c("csv2")) {
    df <- readr::read_csv2(file, ...)
  } else if (ext %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(file, ...)
  } else {
    stop("Unsupported file format. Use txt, tsv, csv, csv2, xls, or xlsx.")
  }

  # Check if the wavenumber column exists
  if (!wn_col %in% names(df)) {
    stop(paste("Column", wn_col, "not found in the file. Please check the correct column name."))
  }

  # Set the wavenumber column
  set_spec_wn(wn_col)

  message("File successfully loaded! Wavenumber column set to: ", wn_col)

  return(df)
}
