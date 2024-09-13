#' Create an Interactive Plot for Spectral Data using Plotly
#'
#' This function generates an interactive Plotly plot for spectral data, allowing for the selection of plot type (absorbance or transmittance), x-axis direction, and plot geometry (points or lines).
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
#' @return A `plotly` object representing the interactive spectral plot.
#'
#' @importFrom plotly plot_ly layout
#' @importFrom dplyr all_of filter %>%
#' @importFrom tidyr pivot_longer
#' @importFrom glue glue
#'
#' @export

spec_smartplotly <- function(.data,
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

  plotly_plot <- plot_data %>%
    plotly::plot_ly(x = ~ .data[[wn_col]], y = ~vals,
                    type = 'scatter',
                    mode = ifelse(geom == "line", "lines", "markers"),
                    color = ~spectra,
                    alpha = alpha) %>%
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
