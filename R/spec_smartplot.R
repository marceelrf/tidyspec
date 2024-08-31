#' Create a Custom Plot for Spectral Data
#'
#' This function generates a customizable plot for spectral data, allowing for the selection of plot type (absorbance or transmittance), x-axis direction, and plot geometry (points or lines).
#'
#' @param .data A `data.frame` or `tibble` containing spectral data.
#' @param wn_col A character string specifying the column name for the wavelength or wavenumber data. Default is `"Wn"`.
#' @param type A character string specifying the type of data to plot. Choices are `"absorbance"` or `"transmittance"`.
#' @param xdir A character string specifying the direction of the x-axis. Choices are `"reverse"` for reverse direction (typically used for wavenumber) or `"standard"` for standard direction.
#' @param geom A character string specifying the geometry of the plot. Choices are `"point"` for a scatter plot or `"line"` for a line plot.
#' @param xmin A numeric value specifying the minimum x-axis value for the plot. Default is 400.
#' @param xmax A numeric value specifying the maximum x-axis value for the plot. Default is 4000.
#' @param alpha A numeric value specifying the transparency level of the plotted points or lines. Default is 0.8.
#'
#' @return A `ggplot` object representing the customized spectral plot.
#'
#' @importFrom ggplot2 ggplot aes scale_x_continuous scale_color_viridis_d xlab ylab theme element_text element_rect element_line element_blank geom_line geom_point scale_x_reverse
#' @importFrom dplyr all_of filter
#' @importFrom tidyr pivot_longer

spec_smartplot <- function(.data,
                           wn_col = "Wn",
                           type = c("absorbance", "transmittance"),
                           xdir = c("reverse", "standard"),
                           geom = c("point", "line"),
                           xmin = 400,
                           xmax = 4000,
                           alpha = 0.8) {

  type <- match.arg(type)
  xdir <- match.arg(xdir)
  geom <- match.arg(geom)

  plot_data <- .data %>%
    tidyr::pivot_longer(cols = -dplyr::all_of(wn_col),
                        names_to = "spectra",
                        values_to = "vals") %>%
    dplyr::filter(.data[[wn_col]] <= xmax, .data[[wn_col]] >= xmin)

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[wn_col]], y = vals, col = spectra)) +
    ggplot2::scale_x_continuous(limits = c(xmin, xmax)) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::xlab(expression(Wavenumber~(cm^-1))) +
    ggplot2::ylab(type) +
    ggplot2::theme(text = ggplot2::element_text(family = "serif"),
                   panel.background = ggplot2::element_rect(fill = "white", linetype = "solid"),
                   panel.grid.major = ggplot2::element_line(colour = "black", size = 0.05),
                   panel.grid.minor = ggplot2::element_line(colour = "grey50", size = 0.01),
                   panel.border = ggplot2::element_rect(linetype = "solid", fill = NA, size = 1))

  if (xdir == "reverse") {
    p <- p + ggplot2::scale_x_reverse()
  }

  if (geom == "line") {
    p <- p + ggplot2::geom_line(alpha = alpha, size = 1.25)
  } else {
    p <- p + ggplot2::geom_point(alpha = alpha, size = 2)
  }

  return(p)
}
