#' Plot Spectral Data
#'
#' Plot spectral data with the given parameters using ggplot2. The spectral data is plotted as either absorbance or transmittance, using either a line or a point plot, with a selectable color map, and can be reversed on the x-axis.
#'
#' @param .data data.frame to plot
#' @param wn_col character string, the column name of the wavenumber
#' @param type character string, plot type "absorbance" or "transmittance"
#' @param xdir character string, plot direction "reverse" or "standard"
#' @param geom character string, plot style "point" or "line"
#' @param xmin numeric, lower bound of the x-axis
#' @param xmax numeric, upper bound of the x-axis
#' @param alpha numeric, plot transparency between 0 and 1
#'
#' @return A ggplot object with the specified parameters
#' @export


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
