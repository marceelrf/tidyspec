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
