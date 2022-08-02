library(tidyverse)
library(viridis)

alg <- readxl::read_excel(path = "Alg.xlsx",col_names = F)


#Vis data
alg %>%
  ggplot(aes(x = Wn, y = Trans)) +
  geom_point()

alg %>%
  mutate(spec = Trans) %>%
  pivot_longer(cols = -1,names_to = "spectra",values_to = "trans") %>%
  ggplot(aes(x = Wn, y = trans,cols = spectra)) +
  geom_point()

#smartplot
smartplot <- function(.data,
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
      ggplot(aes(x = Wn, y = vals,col = spectra)) +
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

#Test
alg %>%
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71) %>%
  spec_trans2abs() %>%
  spec_smooth_avg() %>%
  dplyr::filter(Wn<=1775, Wn >1200) %>%
  #spec_norm_01() %>%
  #spec_norm_var() %>%
  spec_smartplotly(type = "transmittance",
            alpha = 1)

