#' Plot Spectral Data
#'
#' This function is used to create a interactive plot of spectral data. The user can specify the type of spectra (absorbance or transmittance), the wavenumber column, and the minimum and maximum x-axis values.
#'
#' @param .data A data frame containing the spectra data.
#' @param wn_col A character string specifying the name of the wavenumber column in the data.
#' @param type A character string that specifies the type of spectra (absorbance or transmittance).
#' @param xdir A character string that specifies the direction of the x-axis (reverse or standard).
#' @param geom A character string that specifies the type of plot geometry to use (point or line).
#' @param xmin Numeric value that specifies the minimum x-axis value.
#' @param xmax Numeric value that specifies the maximum x-axis value.
#' @param alpha A numeric value specifying the alpha value for the plot.
#' @return A interactive plotly plot of the spectra data.
#' @import plotly
#' @import dplyr
#' @import ggplot2
#' @import glue
#' @export
#' @examples
#' spec_smartplotly(spectra_data,wn_col="Wn",type="absorbance",xmin=400,xmax=4000)

spec_smartplotly <- function(.data,
                           wn_col = "Wn",
                           type = c("absorbance","transmittance"),
                           xdir = c("reverse","standard"),
                           geom = c("point","line"),
                           xmin = 400,
                           xmax = 4000,
                           alpha = .8) {
  require(ggplot2)
  require(dplyr)
  require(plotly)
  require(glue)

  suppressMessages(
    .data %>%
      pivot_longer(cols = -1,
                   names_to = "spectra",
                   values_to = "vals") %>%
      dplyr::filter(.[[1]] <= xmax, .[[1]] >= xmin) %>%
      plot_ly(x = ~ .data[[wn_col]], y = ~vals,
              type = 'scatter',
              mode = 'lines',
              color = ~spectra) %>%
      layout(title = glue("Interactive plot of {type} spectra"),
             xaxis = list(title = glue("Wavenumber"),
                          autorange = "reversed"),
             yaxis = list(title = glue("{type} values")),
             legend = list(title = list(text='Spectra')))


  )
}
