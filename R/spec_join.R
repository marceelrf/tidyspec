#' Join multiple spectral tibbles
#'
#' @description
#' `spec_join()` combines two or more spectral tibbles that share the same
#' wavenumber column. The function performs a full join operation, keeping
#' all wavenumber values from all input tibbles.
#'
#' @param ... Two or more tibbles containing spectral data
#' @param wn_col Character string specifying the wavenumber column name.
#'   If NULL, uses the globally set wavenumber column from `set_spec_wn()`.
#' @param join_type Character string specifying the type of join operation.
#'   Options: "full" (default), "inner", "left". See Details.
#' @param suffix Character vector of length 2 specifying suffixes to add
#'   to non-wavenumber columns when there are naming conflicts.
#'   Default: c(".x", ".y")
#'
#' @importFrom stats setNames
#'
#' @details
#' Join types:
#' - "full": Keep all wavenumber values from all tibbles (default)
#' - "inner": Keep only wavenumber values present in all tibbles
#' - "left": Keep wavenumber values from the first tibble
#'
#' The function will show warnings about the wavenumber column being used
#' (similar to other tidyspec functions) and will handle missing values
#' appropriately.
#'
#' @return A tibble containing the joined spectral data
#'
#' @examples
#' \dontrun{
#' # Set wavenumber column
#' set_spec_wn("Wavenumber")
#'
#' # Join two spectral datasets
#' joined_data <- spec_join(CoHAspec, other_spec_data)
#'
#' # Join multiple datasets with inner join
#' joined_data <- spec_join(spec1, spec2, spec3, join_type = "inner")
#'
#' # Specify wavenumber column explicitly
#' joined_data <- spec_join(spec1, spec2, wn_col = "Wavenumber")
#' }
#'
#' @export
spec_join <- function(..., wn_col = NULL, join_type = "full", suffix = c(".x", ".y")) {

  # Collect all tibbles from ...
  spec_list <- list(...)

  # Check if we have at least 2 tibbles
  if (length(spec_list) < 2) {
    stop("At least 2 tibbles are required for joining.", call. = FALSE)
  }

  # Check if all inputs are data frames/tibbles
  if (!all(sapply(spec_list, function(x) is.data.frame(x)))) {
    stop("All inputs must be data frames or tibbles.", call. = FALSE)
  }

  # Get wavenumber column
  if (is.null(wn_col)) {
    wn_col <- getOption("tidyspec.wn_col")
    if (is.null(wn_col)) {
      stop("No wavenumber column specified. Use wn_col argument or set_spec_wn().",
           call. = FALSE)
    }
    # Show warning message (similar to other tidyspec functions)
    message_once(paste0("wn_col not specified. Using default value: ", wn_col, "."))
  }

  # Check if wavenumber column exists in all tibbles
  wn_col_exists <- sapply(spec_list, function(x) wn_col %in% names(x))
  if (!all(wn_col_exists)) {
    missing_wn <- which(!wn_col_exists)
    stop(paste0("Wavenumber column '", wn_col, "' not found in tibble(s): ",
                paste(missing_wn, collapse = ", ")), call. = FALSE)
  }

  # Validate join_type
  if (!join_type %in% c("full", "inner", "left")) {
    stop("join_type must be one of: 'full', 'inner', 'left'", call. = FALSE)
  }

  # Start with first tibble
  result <- spec_list[[1]]

  # Join with remaining tibbles
  for (i in 2:length(spec_list)) {
    current_tibble <- spec_list[[i]]

    # Perform the join based on join_type
    if (join_type == "full") {
      result <- dplyr::full_join(result, current_tibble,
                                 by = wn_col, suffix = suffix)
    } else if (join_type == "inner") {
      result <- dplyr::inner_join(result, current_tibble,
                                  by = wn_col, suffix = suffix)
    } else if (join_type == "left") {
      result <- dplyr::left_join(result, current_tibble,
                                 by = wn_col, suffix = suffix)
    }
  }

  # Sort by wavenumber column
  result <- result[order(result[[wn_col]]), ]

  # Convert back to tibble and return
  tibble::as_tibble(result)
}

# Helper function for showing messages only once (if not already in tidyspec)
message_once <- function(message, id = message) {
  last_message_time <- getOption(paste0("tidyspec.last_message.", digest::digest(id)))
  current_time <- Sys.time()

  if (is.null(last_message_time) ||
      difftime(current_time, last_message_time, units = "hours") >= 2) {
    message(message, "\nThis message is shown at most once every 2 hours.")
    options(setNames(list(current_time), paste0("tidyspec.last_message.", digest::digest(id))))
  }
}
