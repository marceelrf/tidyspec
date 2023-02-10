library(tidyverse)
library(viridis)
library(extrafont)

data <- readxl::read_xlsx(path = "data/alginate_prp.xlsx")

data %>% spec_smartplot()


data %>%
  filter(Wn<1800, Wn>1475) %>%
  spec_smartplot()


reg1_algprp<- data %>%
  dplyr::filter(Wn<1800, Wn>1485) %>%
  spec_smooth_sga() %>%
  spec_blc_rollingBall(Wn_max = 1800,Wn_min = 1485,wm = 75,ws = 55) %>%
  spec_norm_01() %>%
  filter(Wn < 1763)

#plot
reg1_algprp %>%
  pivot_longer(cols = -Wn,names_to = "Syn",values_to = "Abs") %>%
  mutate(Syn = factor(Syn,levels = c("Alginate",LETTERS[1:5]))) %>%
  ggplot(aes(x = Wn, y = Abs, col = Syn)) +
  geom_line(size = 1.3) +
  scale_x_reverse(n.breaks = 10) +
  scale_color_manual(values = c("black",viridis(n = 5,direction = -1))) +
  theme_bw() +
  labs(title = "Wavenumber: 1800 cm-1 to 1485 cm-1",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/reg1.png",scale = 1.5,dpi = 600)
write_csv(reg1_algprp,file =  "AlgPRP/reg1.csv")
