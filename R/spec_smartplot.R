#' Plot spectral data
#'
#' Plot spectral data with the given parameters. The plot is created using ggplot2. The spectral data is plotted as either absorbance or transmittance, using either a line or a point plot, with a selectable color map, and can be reversed on the x-axis. The plot is displayed with a white background, text with a serif font, major and minor gridlines, a border, and labeled axes.
#'
#' @param .data data.frame to plot
#' @param wn_col character string column name of the wavenumber
#' @param type character string, plot type "absorbance" or "transmittance"
#' @param xdir character string, plot direction "reverse" or "standard"
#' @param geom character string, plot style "point" or "line"
#' @param xmin numeric, lower bound of x axis
#' @param xmax numeric, upper bound of x axis
#' @param alpha numeric, plot transparency between 0 and 1
#'
#' @return plot with specified parameters
#' @export
#'
#' @examples
#' spec_smartplot(.data = iris, wn_col = "Sepal.Length",
#' type = "transmittance", xdir = "standard",
#' geom = "line", xmin = 3, xmax = 8, alpha = .8)

spec_smartplot <- function(.data,
                           wn_col = "Wn",
                      type = c("absorbance","transmittance"),
                      xdir = c("reverse","standard"),
                      geom = c("point","line"),
                      xmin = 400,
                      xmax = 4000,
                      alpha = .8) {
  require(ggplot2)
  require(dplyr)

  suppressMessages(
    .data %>%
      pivot_longer(cols = -1,
                   names_to = "spectra",
                   values_to = "vals") %>%
      dplyr::filter(.[[1]] <= xmax, .[[1]] >= xmin) %>%
      ggplot(aes(x = .data[[wn_col]], y = vals,col = spectra)) +
      geom_line(alpha = alpha,size = 1.25) +
      scale_x_reverse() +
      scale_color_viridis(discrete = T) +
      xlab(expression(Wavenumber (cm^-1))) +
      ylab(type) +
      theme(text = element_text(family = "serif"),
            panel.background = element_rect(fill = "white",
                                            linetype = "solid"),
            panel.grid.major = element_line(colour = "black",
                                            size = .05),
            panel.grid.minor = element_line(colour = "grey50",
                                            size = .01),
            panel.border = element_rect(linetype = "solid",
                                        fill = NA,size = 1))

  )
}
