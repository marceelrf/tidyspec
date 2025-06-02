tidyspec_env <- new.env(parent = emptyenv())

warn_wn_col_default <- function(wn_col) {
  option_name <- "tidyspec.last_wn_col_warning"
  current_time <- Sys.time()
  last_time <- getOption(option_name)

  if (is.null(last_time) || difftime(current_time, last_time, units = "hours") > 2) {
    options(tidyspec.last_wn_col_warning = current_time)

    # crayon style
    if (requireNamespace("crayon", quietly = TRUE)) {
      time_str <- crayon::red$bold("2 hours")
    } else {
      time_str <- "2 hours"
    }

    warning(sprintf(
      "wn_col not provided. Using defined default: '%s'.\nThis message is shown at most once every %s.",
      wn_col, time_str
    ))
  }
}

warn_missing_param_once <- function(param_name, default_value, extra_msg = NULL) {

  if (requireNamespace("crayon", quietly = TRUE)) {
    time_str <- crayon::red$bold("2 hours")
    param_str <- crayon::yellow$bold(param_name)
  } else {
    time_str <- "2 hours"
    param_str <- param_name
  }

  option_name <- paste0("tidyspec.last_warning_", param_name)
  current_time <- Sys.time()
  last_time <- getOption(option_name)

  if (is.null(last_time) || difftime(current_time, last_time, units = "hours") > 2) {
    options(structure(list(current_time), .Names = option_name))

    full_msg <- sprintf(
      "%s not specified. Using default value: %s.\nThis message is shown at most once every %s.",
      param_str,
      format(default_value),
      time_str
    )
    if (!is.null(extra_msg)) {
      full_msg <- paste(full_msg, extra_msg, sep = "\n")
    }

    warning(full_msg, call. = FALSE)
  }
}
