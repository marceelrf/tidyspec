#' Create a Custom Plot for Spectral Data
#'
#' This function generates a customizable plot for spectral data, allowing for the selection of plot type (absorbance or transmittance), x-axis direction, and plot geometry (points or lines).
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength or wavenumber data. This parameter is required.
#' @param xdir A character string specifying the direction of the x-axis. Choices are `"reverse"` for reverse direction (typically used for wavenumber) or `"standard"` for standard direction.
#' @param geom A character string specifying the geometry of the plot. Choices are `"point"` for a scatter plot or `"line"` for a line plot.
#' @param xmin A numeric value specifying the minimum x-axis value for the plot. If not provided, the minimum value from the `wn_col` data will be used.
#' @param xmax A numeric value specifying the maximum x-axis value for the plot. If not provided, the maximum value from the `wn_col` data will be used.
#' @param alpha A numeric value specifying the transparency level of the plotted points or lines. Default is 0.8.
#' @param type A character string specifying the y-labes as transmittance or absorbance. Default is absorbance.
#' @return A `ggplot` object representing the customized spectral plot (absorbance or transmittance as a function of wavelength/wavenumber).
#'
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_color_viridis_d xlab ylab theme element_text element_rect element_line element_blank geom_line geom_point scale_x_reverse
#' @importFrom dplyr all_of filter %>%
#' @importFrom tidyr pivot_longer
#'
#' @export

spec_smartplot <- function(.data,
                           wn_col = NULL,
                           xdir = c("reverse", "standard"),
                           geom = c("point", "line"),
                           xmin = NULL,
                           xmax = NULL,
                           alpha = 0.8,
                           type = c("absorbance", "transmittance")) {


  if (!inherits(.data, "data.frame")) {
    stop("The argument .data must be a data.frame or tibble.")
  }

  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no default defined with set_spec_wn().")
    } else {
      warn_missing_param_once("wn_col", wn_col)
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
    warn_missing_param_once("xmin", xmin)
  } else if (!is.numeric(xmin)) {
    warning("xmin must be numeric. Ignoring provided xmin.")
    xmin <- min(wn_values, na.rm = TRUE)
  }

  if (is.null(xmax)) {
    xmax <- max(wn_values, na.rm = TRUE)
    warn_missing_param_once("xmax", xmax)
  } else if (!is.numeric(xmax)) {
    warning("xmax must be numeric. Ignoring provided xmax.")
    xmax <- max(wn_values, na.rm = TRUE)
  }

  if (xmin >= xmax) {
    stop("xmin must be less than xmax.")
  }

  xdir <- match.arg(xdir)
  geom <- match.arg(geom)
  type <- match.arg(type)

  if (!is.numeric(alpha) || alpha < 0 || alpha > 1) {
    warning("alpha must be numeric between 0 and 1. Using default value 0.8.")
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

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[wn_col]], y = vals, col = spectra)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::xlab(expression(Wavenumber~(cm^-1))) +
    ggplot2::ylab(type) +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"),
                   panel.background = ggplot2::element_rect(fill = "white", linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(colour = "black", linewidth = 0.05),
                   panel.grid.minor = ggplot2::element_line(colour = "grey50", linewidth = 0.01),
                   panel.border = ggplot2::element_rect(linetype = "solid", fill = NA, linewidth = 1)) +
    ggplot2::labs(col = "spectra")

  if (xdir == "reverse") {
    p <- p + ggplot2::scale_x_reverse(limits = c(xmax, xmin))
  } else {
    p <- p + ggplot2::scale_x_continuous(limits = c(xmin, xmax))
  }

  if (geom == "line") {
    p <- p + ggplot2::geom_line(alpha = alpha, linewidth = 1.25)
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha)
  }

  return(p)
}
