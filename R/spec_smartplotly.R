#' Plot Spectral Data
#'
#' Create an interactive plot of spectral data using `plotly`. The user can specify the type of spectra (absorbance or transmittance), the wavenumber column, and the minimum and maximum x-axis values.
#'
#' @param .data A data frame containing the spectra data.
#' @param wn_col A character string specifying the name of the wavenumber column in the data.
#' @param type A character string specifying the type of spectra ("absorbance" or "transmittance").
#' @param xdir A character string specifying the direction of the x-axis ("reverse" or "standard").
#' @param geom A character string specifying the type of plot geometry to use ("point" or "line").
#' @param xmin Numeric value specifying the minimum x-axis value.
#' @param xmax Numeric value specifying the maximum x-axis value.
#' @param alpha A numeric value specifying the transparency level for the plot.
#' @return An interactive `plotly` plot of the spectra data.
#' @import plotly
#' @import dplyr
#' @import ggplot2
#' @import glue
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
