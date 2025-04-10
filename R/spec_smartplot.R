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
  if (is.null(wn_col)) {
    wn_col <- get0(".wn_col_default", envir = tidyspec_env,
                   ifnotfound = NULL)
    if (is.null(wn_col)) {
      stop("wn_col not specified and no pattern defined with set_spec_wn()")
    }
  }

  wn_values <- .data[[wn_col]]

  if (is.null(xmin)) {
    xmin <- min(wn_values)
  }
  if (is.null(xmax)) {
    xmax <- max(wn_values)
  }

  xdir <- match.arg(xdir)
  geom <- match.arg(geom)
  type <- match.arg(type)

  plot_data <- .data %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(wn_col),
                        names_to = "spectra",
                        values_to = "vals") %>%
    dplyr::filter(.data[[wn_col]] <= xmax, .data[[wn_col]] >= xmin)

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
