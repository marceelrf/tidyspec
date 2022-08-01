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
