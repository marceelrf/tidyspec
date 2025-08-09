#' Extract spectral values at specific wavenumbers
#'
#' @description
#' `spec_values_at()` extracts spectral values at specified wavenumber positions.
#' The function finds the closest wavenumber matches and returns the corresponding
#' spectral intensities for each sample column.
#'
#' @param spec_data A tibble containing spectral data
#' @param wn_values Numeric vector of wavenumber values to extract
#' @param wn_col Character string specifying the wavenumber column name.
#'   If NULL, uses the globally set wavenumber column from `set_spec_wn()`.
#' @param method Character string specifying the extraction method.
#'   Options: "closest" (default), "exact", "interpolate". See Details.
#' @param tolerance Numeric value specifying the maximum allowed difference
#'   for "closest" method. Default: Inf (no limit).
#' @param format Character string specifying output format.
#'   Options: "wide" (default), "long". See Details.
#'
#' @importFrom stats setNames
#' @details
#' Extraction methods:
#' - "closest": Find the closest wavenumber match (default)
#' - "exact": Only return exact matches (NA for non-matches)
#' - "interpolate": Use linear interpolation between adjacent points
#'
#' Output formats:
#' - "wide": Each wavenumber becomes a row, samples as columns
#' - "long": Tidy format with wavenumber, sample, and value columns
#'
#' @return A tibble with extracted values in the specified format
#'
#' @examples
#' \dontrun{
#' # Set wavenumber column
#' set_spec_wn("Wavenumber")
#'
#' # Extract values at specific wavenumbers
#' peaks <- c(1650, 1450, 1250, 1050)
#' extracted <- spec_values_at(CoHAspec, peaks)
#'
#' # Use exact matching only
#' extracted <- spec_values_at(CoHAspec, peaks, method = "exact")
#'
#' # Get results in long format
#' extracted <- spec_values_at(CoHAspec, peaks, format = "long")
#'
#' # Use interpolation
#' extracted <- spec_values_at(CoHAspec, peaks, method = "interpolate")
#' }
#'
#' @export
spec_values_at <- function(spec_data, wn_values, wn_col = NULL,
                         method = "closest", tolerance = Inf, format = "wide") {

  # Validate inputs
  if (!is.data.frame(spec_data)) {
    stop("spec_data must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.numeric(wn_values)) {
    stop("wn_values must be a numeric vector.", call. = FALSE)
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

  # Check if wavenumber column exists
  if (!wn_col %in% names(spec_data)) {
    stop(paste0("Wavenumber column '", wn_col, "' not found in spec_data."),
         call. = FALSE)
  }

  # Validate method
  if (!method %in% c("closest", "exact", "interpolate")) {
    stop("method must be one of: 'closest', 'exact', 'interpolate'", call. = FALSE)
  }

  # Validate format
  if (!format %in% c("wide", "long")) {
    stop("format must be one of: 'wide', 'long'", call. = FALSE)
  }

  # Get wavenumber and spectral columns
  wn_data <- spec_data[[wn_col]]
  spec_cols <- names(spec_data)[names(spec_data) != wn_col]

  # Initialize result list
  result_list <- list()

  # Extract values for each requested wavenumber
  for (target_wn in wn_values) {

    if (method == "exact") {
      # Exact match only
      match_idx <- which(wn_data == target_wn)
      if (length(match_idx) == 0) {
        # No exact match found
        extracted_values <- rep(NA, length(spec_cols))
        names(extracted_values) <- spec_cols
      } else {
        # Take first match if multiple
        extracted_values <- as.numeric(spec_data[match_idx[1], spec_cols])
        names(extracted_values) <- spec_cols
      }

    } else if (method == "closest") {
      # Find closest match
      distances <- abs(wn_data - target_wn)
      min_distance <- min(distances)

      if (min_distance <= tolerance) {
        closest_idx <- which.min(distances)
        extracted_values <- as.numeric(spec_data[closest_idx, spec_cols])
        names(extracted_values) <- spec_cols
      } else {
        # Outside tolerance
        extracted_values <- rep(NA, length(spec_cols))
        names(extracted_values) <- spec_cols
      }

    } else if (method == "interpolate") {
      # Linear interpolation
      extracted_values <- numeric(length(spec_cols))
      names(extracted_values) <- spec_cols

      for (col in spec_cols) {
        y_values <- spec_data[[col]]
        # Remove NAs for interpolation
        valid_idx <- !is.na(y_values) & !is.na(wn_data)

        if (sum(valid_idx) >= 2) {
          interpolated <- approx(x = wn_data[valid_idx],
                                 y = y_values[valid_idx],
                                 xout = target_wn,
                                 rule = 1)  # NA outside range
          extracted_values[col] <- interpolated$y
        } else {
          extracted_values[col] <- NA
        }
      }
    }

    result_list[[as.character(target_wn)]] <- extracted_values
  }

  # Convert to tibble format
  if (format == "wide") {
    # Wide format: wavenumbers as rows, samples as columns
    result_df <- do.call(rbind, result_list)
    result_df <- tibble::as_tibble(result_df)
    result_df[[wn_col]] <- as.numeric(rownames(result_df))
    # Reorder columns to put wavenumber first
    result_df <- result_df[, c(wn_col, spec_cols)]

  } else {
    # Long format: tidy data with wavenumber, sample, value columns
    result_long <- data.frame()

    for (i in seq_along(result_list)) {
      target_wn <- as.numeric(names(result_list)[i])
      values <- result_list[[i]]

      temp_df <- data.frame(
        wavenumber = rep(target_wn, length(values)),
        sample = names(values),
        value = as.numeric(values),
        stringsAsFactors = FALSE
      )

      result_long <- rbind(result_long, temp_df)
    }

    # Rename wavenumber column to match wn_col
    names(result_long)[1] <- wn_col
    result_df <- tibble::as_tibble(result_long)
  }

  return(result_df)
}

# Helper function for showing messages only once (if not already in tidyspec)
message_once <- function(message, id = message) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    # Fallback if digest not available
    message(message, "\nThis message is shown at most once every 2 hours.")
    return()
  }

  last_message_time <- getOption(paste0("tidyspec.last_message.", digest::digest(id)))
  current_time <- Sys.time()

  if (is.null(last_message_time) ||
      difftime(current_time, last_message_time, units = "hours") >= 2) {
    message(message, "\nThis message is shown at most once every 2 hours.")
    options(setNames(list(current_time), paste0("tidyspec.last_message.", digest::digest(id))))
  }
}
