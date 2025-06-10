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
#' \donttest{
#'   file_path <- system.file("extdata", "CoHAspec.csv", package = "tidyspec")
#'   df <- spec_read(file_path, wn_col = "Wavenumber")
#'   check_wn_col() # Verifica se a coluna de wavenumber está definida
#' }
#' @export
spec_read <- function(file, wn_col, ...) {
  # Check if the file exists
  if (!file.exists(file)) {
    stop("The specified file does not exist: ", file)
  }

  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("txt", "tsv")) {
    df <- tryCatch(
      readr::read_tsv(file, ...),
      error = function(e) {
        stop("Error reading TSV/TXT file: ", e$message)
      }
    )
  } else if (ext == "csv") {
    df <- tryCatch(
      readr::read_csv(file, ...),
      error = function(e) {
        stop("Error reading CSV file: ", e$message)
      }
    )
  } else if (ext == "csv2") {
    df <- tryCatch(
      readr::read_csv2(file, ...),
      error = function(e) {
        stop("Error reading CSV2 file: ", e$message)
      }
    )
  } else if (ext %in% c("xls", "xlsx")) {
    df <- tryCatch(
      readxl::read_excel(file, ...),
      error = function(e) {
        stop("Error reading Excel file: ", e$message)
      }
    )
  } else {
    stop("Unsupported file format. Use txt, tsv, csv, csv2, xls, or xlsx.")
  }

  if (nrow(df) == 0) {
    warning("File was loaded but is empty (0 rows). Please check the file.")
  }

  if (!wn_col %in% names(df)) {
    stop(paste0("Column '", wn_col, "' was not found in the file. Please check the column name."))
  }

  if (all(is.na(df[[wn_col]]))) {
    warning(paste0("Column '", wn_col, "' exists, but all values are NA."))
  }

  set_spec_wn(wn_col)

  message("File loaded successfully! Wavenumber column set to: ", wn_col)

  return(df)
}
