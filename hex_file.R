library(RcppFaddeeva)
library(tidyverse)
library(hexSticker)

# b4 = .8*Voigt(x = x,x0 = 1300,sigma = 150,gamma = 2.5),
# b5 = .75*Voigt(x = x,x0 = 1345,sigma = 150,gamma = 2.5),
# b6 = .8*Voigt(x = x,x0 = 1410,sigma = 200,gamma = 2.5)
original_vector <- seq(1000, 2000, by = 2)
slope <- .0115
slope_vector <- slope * (1:length(original_vector))/2000


(
  p <- tibble(x=original_vector,
       b1 = Voigt(x = x,x0 = 1200,sigma = 100,gamma = 2.5),
       b2 = 1.15*Voigt(x = x,x0 = 1205,sigma = 100,gamma = 2.5),
       b3 = 1.3*Voigt(x = x,x0 = 1210,sigma = 100,gamma = 2.5),
       b4 = 1.4*Voigt(x = x,x0 = 1215,sigma = 100,gamma = 1.5),
       b5 = 1.7*Voigt(x = x,x0 = 1230,sigma = 110,gamma = .5)
      ) %>%
  mutate(across(starts_with("b"),\(x) x + slope_vector)) %>%
  pivot_longer(cols = -x) %>%
  ggplot(aes(x=x,y = value,col = name)) +
  geom_line(linewidth=1) +
  scale_color_viridis_d(option = "F",direction = -1) +
  theme_void() + theme_transparent() + theme(legend.position = "none")
  # theme(panel.grid = element_line(colour = "grey25",linewidth = .01),
  #       panel.background = element_rect(fill = "royalblue",
  #                                       #colour = "black"
  #                                       ),
  #       legend.position = "none" )

)
sticker(p, package="tidyspec",
        p_size=40, s_x=1.1, s_y=.75, s_width=1.5, s_height=.8,
        h_fill = "royalblue",
        h_color = "navy",
        filename="tidyspec_ggplot2.png",dpi = 600)


(
  p <- tibble(x=original_vector,
              b1 = Voigt(x = x,x0 = 1200,sigma = 100,gamma = 2.5) +
                .3*Voigt(x = x,x0 = 1800,sigma = 50,gamma = 50),
              b2 = 1.15*Voigt(x = x,x0 = 1205,sigma = 100,gamma = 2.5) +
                .25*Voigt(x = x,x0 = 1805,sigma = 50,gamma = 50),
              b3 = 1.3*Voigt(x = x,x0 = 1210,sigma = 100,gamma = 2.5) +
                .225*Voigt(x = x,x0 = 1810,sigma = 50,gamma = 50),
              b4 = 1.4*Voigt(x = x,x0 = 1215,sigma = 100,gamma = 1.5) +
                .215*Voigt(x = x,x0 = 1815,sigma = 50,gamma = 50),
              b5 = 1.7*Voigt(x = x,x0 = 1230,sigma = 110,gamma = .5) +
                .105*Voigt(x = x,x0 = 1830,sigma = 50,gamma = 50)
  ) %>%
    mutate(across(starts_with("b"),\(x) x + slope_vector)) %>%
    pivot_longer(cols = -x) %>%
    ggplot(aes(x=x,y = value,col = name)) +
    geom_line(linewidth=1) +
    scale_color_viridis_d(option = "F",direction = -1) +
    theme_void() + theme_transparent() + theme(legend.position = "none")
  # theme(panel.grid = element_line(colour = "grey25",linewidth = .01),
  #       panel.background = element_rect(fill = "royalblue",
  #                                       #colour = "black"
  #                                       ),
  #       legend.position = "none" )

)

sticker(p, package="tidyspec",
        p_size=20, s_x=1.1, s_y=.75, s_width=1.5, s_height=.8,
        h_fill = "royalblue",
        h_color = "navy",
        filename="tidyspec_ggplot2_v2.png")
