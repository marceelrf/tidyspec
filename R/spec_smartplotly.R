#' Create an Interactive Plot for Spectral Data using Plotly
#'
#' This function generates an interactive Plotly plot for spectral data, allowing for the selection of plot type (absorbance or transmittance), x-axis direction, and plot geometry (points or lines).
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength or wavenumber data. Default is `"Wn"`.
#' @param type A character string specifying the type of data to plot. Choices are `"absorbance"` or `"transmittance"`.
#' @param xdir A character string specifying the direction of the x-axis. Choices are `"reverse"` for reverse direction (typically used for wavenumber) or `"standard"` for standard direction.
#' @param geom A character string specifying the geometry of the plot. Choices are `"point"` for a scatter plot or `"line"` for a line plot.
#' @param xmin A numeric value specifying the minimum x-axis value for the plot. If not provided, the minimum value from the `wn_col` data will be used.
#' @param xmax A numeric value specifying the maximum x-axis value for the plot. If not provided, the maximum value from the `wn_col` data will be used.
#' @param alpha A numeric value specifying the transparency level of the plotted points or lines. Default is 0.8.
#'
#' @return A `plotly` object representing the interactive spectral plot.
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr all_of filter %>%
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#'
#' @export

spec_smartplotly <- function(.data,
                             wn_col = NULL,
                             type = c("absorbance", "transmittance"),
                             xdir = c("reverse", "standard"),
                             geom = c("point", "line"),
                             xmin = NULL,
                             xmax = NULL,
                             alpha = 0.8) {

  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env, ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no default defined with set_spec_wn().")
    } else {
      warning(sprintf("wn_col not provided. Using the defined default: '%s'.", wn_col))
    }
  }

  if (!(wn_col %in% names(.data))) {
    stop(sprintf("Column '%s' was not found in the dataset.", wn_col))
  }

  wn_values <- .data[[wn_col]]

  if (!is.numeric(wn_values)) {
    stop(sprintf("Column '%s' must be numeric for plotting.", wn_col))
  }

  if (is.null(xmin)) {
    xmin <- min(wn_values, na.rm = TRUE)
    warning(sprintf("xmin not specified. Using minimum value from column '%s': %f", wn_col, xmin))
  } else if (!is.numeric(xmin)) {
    warning("xmin must be numeric. Using the minimum value from the wn_col column.")
    xmin <- min(wn_values, na.rm = TRUE)
  }

  if (is.null(xmax)) {
    xmax <- max(wn_values, na.rm = TRUE)
    warning(sprintf("xmax not specified. Using maximum value from column '%s': %f", wn_col, xmax))
  } else if (!is.numeric(xmax)) {
    warning("xmax must be numeric. Using the maximum value from the wn_col column.")
    xmax <- max(wn_values, na.rm = TRUE)
  }

  if (xmin >= xmax) {
    stop("xmin must be less than xmax.")
  }

  type <- match.arg(type)
  xdir <- match.arg(xdir)
  geom <- match.arg(geom)

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    warning("alpha must be numeric and between 0 and 1. Using default value 0.8.")
    alpha <- 0.8
  }

  plot_data <- .data %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(wn_col),
                        names_to = "spectra",
                        values_to = "vals") %>%
    dplyr::filter(.data[[wn_col]] >= xmin, .data[[wn_col]] <= xmax)

  if (nrow(plot_data) == 0) {
    warning("No data available after applying xmin and xmax limits. Check the specified values.")
  }

  plotly_plot <- plot_data %>%
    plotly::plot_ly(
      x = ~ .data[[wn_col]], y = ~vals,
      type = 'scatter',
      mode = ifelse(geom == "line", "lines", "markers"),
      color = ~spectra,
      alpha = alpha
    ) %>%
    plotly::layout(
      title = glue::glue("Interactive plot of {type} spectra"),
      xaxis = list(
        title = "Wavenumber (cm^-1)",
        autorange = ifelse(xdir == "reverse", "reversed", "normal")
      ),
      yaxis = list(title = glue::glue("{type} values")),
      legend = list(title = list(text = 'Spectra')),
      plot_bgcolor = 'white'
    )

  return(plotly_plot)
}
