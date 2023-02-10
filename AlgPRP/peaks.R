library(tidyverse)
library(viridis)
library(extrafont)
library(RcppFaddeeva)

files_bf <- list.files(path = "AlgPRP/",pattern = "best_fit",full.names = T)

tmp <- files_bf %>%
  map(read_csv) %>%
  tibble() %>%
  mutate(spec = files_bf) %>%
  mutate(spec = str_replace(spec,pattern = "AlgPRP/",replacement = "")) %>%
  mutate(spec = str_replace(spec,pattern = "_best_fit.csv",replacement = "")) %>%
  unnest(.) %>%
  select(-`...1`)


# Alginate ----------------------------------------------------------------
peaks_alginate <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 1.02332024438888*Voigt(x = Wn,x0 = 1668.00231238691,sigma = 8.38563917132374,gamma = 6.75)) %>%
  mutate(v2 = 1.00007045630867*Voigt(x = Wn,x0 = 1594.4466843456,sigma = 5.76333826529831,gamma = 7)) %>%
  mutate(v3 = 1.01268464934704*Voigt(x = Wn,x0 = 1583.80108848443,sigma = 4.77152626124953,gamma = 6.75)) %>%
  mutate(v4 = 2.89842167580237*Voigt(x = Wn,x0 = 1606.2663631003,sigma = 12.4387042360277,gamma = 6.75)) %>%
  mutate(v5 = 0.102220519867019*Voigt(x = Wn,x0 = 1725.57747417136,sigma = 0.01000000160838,gamma = 5.25)) %>%
  mutate(v6 = 1.3467097248255*Voigt(x = Wn,x0 = 1688.27436087307,sigma = 10.9673043596835,gamma = 6.25)) %>%
  mutate(v7 = 0.154600201912149*Voigt(x = Wn,x0 = 1711.23925513968,sigma = 1.3045649859609,gamma = 6.25)) %>%
  mutate(v8 = 12.7148804033377*Voigt(x = Wn,x0 = 1636.17320699621,sigma = 21.8026099214273,gamma = 6.5)) %>%
  mutate(v9 = 1.28705858294248*Voigt(x = Wn,x0 = 1571.01634860995,sigma = 7.92062657425815,gamma = 5.75)) %>%
  mutate(v10 = 0.105017738239105*Voigt(x = Wn,x0 = 1551.60254123227,sigma = 2.08390142099285,gamma = 3.25)) %>%
  mutate(v11 = 0.450049601451037*Voigt(x = Wn,x0 = 1488.37709110404,sigma = 12.3786566330054,gamma = 1.75)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:11,sep = "")))

fitAlginate <- tmp %>%
  filter(spec == "alginate") %>%
  mutate(Wn = reg1_algprp$Wn)


reg1_algprp %>%
  ggplot(aes(x = Wn, y = Alginate)) +
  geom_point(alpha = .5) +
  geom_line(data = fitAlginate,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_alginate, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  #scale_y_continuous(n.breaks = 10,limits = c(0,1)) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Alginate",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksAlginate.png",scale = 3,dpi = 600)
# Syn A -------------------------------------------------------------------
peaks_A <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 61.1660677360233*Voigt(x = Wn,x0 = 1650.61229433029,sigma = 23.512127697052,gamma = 5.5)) %>%
  mutate(v2 = 7.28457711516013*Voigt(x = Wn,x0 = 1679.46259844596,sigma = 8.16531840608888,gamma = 10)) %>%
  mutate(v3 = 21.1598214692326*Voigt(x = Wn,x0 = 1544.86917843977,sigma = 16.3869196107336,gamma = 6.75)) %>%
  mutate(v4 = 30.6025134080179*Voigt(x = Wn,x0 = 1596.58165264713,sigma = 27.4596101413953,gamma = 5.75)) %>%
  mutate(v5 = 4.46008731662884*Voigt(x = Wn,x0 = 1517.84005870646,sigma = 7.5677723470859,gamma = 6.25)) %>%
  mutate(v6 = 3.32550895758245*Voigt(x = Wn,x0 = 1692.41065851589,sigma = 1.00002540743349,gamma = 11)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:6,sep = "")))

fitA <- tmp %>%
  filter(spec == "SynA") %>%
  mutate(Wn = reg1_algprp$Wn)

reg1_algprp %>%
  ggplot(aes(x = Wn, y = A)) +
  geom_point(alpha = .5) +
  geom_line(data = fitA,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_A, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Synthesis A",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksSynA.png",scale = 3,dpi = 600)
# Syn B -------------------------------------------------------------------
peaks_B <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 30.0627503278689*Voigt(x = Wn,x0 = 1672.00000000007,sigma = 16.0470101970758,gamma = 6.5)) %>%
  mutate(v2 = 2.50860238108617*Voigt(x = Wn,x0 = 1691.14918023987,sigma = 5.79594293086739,gamma = 6.5)) %>%
  mutate(v3 = 41.3822460412507*Voigt(x = Wn,x0 = 1639.16688173441,sigma = 16.9306930742099,gamma = 8.75)) %>%
  mutate(v4 = 22.56616963*Voigt(x = Wn,x0 = 1552.00782923702,sigma = 14.6673939057931,gamma = 8.75)) %>%
  mutate(v5 = 5.05473913255079*Voigt(x = Wn,x0 = 1515.94087489055,sigma = 6.29007806618582,gamma = 6)) %>%
  mutate(v6 = 20.4761958017349*Voigt(x = Wn,x0 = 1598.10392806347,sigma = 19.3315177334463,gamma = 10)) %>%
  mutate(v7 = 6.12707482780454*Voigt(x = Wn,x0 = 1532.52767616434,sigma = 6.94706429121957,gamma = 8.75)) %>%
  mutate(v8 = 3.1246083756941*Voigt(x = Wn,x0 = 1647.60462689473,sigma = 7.5427782907213,gamma = 10.5)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:8,sep = "")))

fitB <- tmp %>%
  filter(spec == "SynB") %>%
  mutate(Wn = reg1_algprp$Wn)

reg1_algprp %>%
  ggplot(aes(x = Wn, y = B)) +
  geom_point(alpha = .5) +
  geom_line(data = fitB,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_B, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Synthesis B",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksSynB.png",scale = 3,dpi = 600)
# Syn C -------------------------------------------------------------------
peaks_C <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 0.0660156283962505*Voigt(x = Wn,x0 = 1729.20439754966,sigma = 0.000887635233401627,gamma = 5.5)) %>%
  mutate(v2 = 0.0000100011757555712*Voigt(x = Wn,x0 = 1712.52071586586,sigma = 0.0540784335831128,gamma = 6.5)) %>%
  mutate(v3 = 25.8221723116022*Voigt(x = Wn,x0 = 1650.57313939318,sigma = 26.66658547995,gamma = 6.5)) %>%
  mutate(v4 = 0.24020032985819*Voigt(x = Wn,x0 = 1593.34738935281,sigma = 1.18112060318076,gamma = 11.5)) %>%
  mutate(v5 = 17.6915047955343*Voigt(x = Wn,x0 = 1595.65520243745,sigma = 27.40105757192,gamma = 7.5)) %>%
  mutate(v6 = 4.01721438818027*Voigt(x = Wn,x0 = 1542.6014200733,sigma = 13.8601094427507,gamma = 7.75)) %>%
  mutate(v7 = 0.854437981131248*Voigt(x = Wn,x0 = 1518.56978281509,sigma = 6.32742936077647,gamma = 5.75)) %>%
  mutate(v8 = 3.4447379047687*Voigt(x = Wn,x0 = 1685.17136869269,sigma = 6.79948533300731,gamma = 14.75)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:8,sep = "")))

fitC <- tmp %>%
  filter(spec == "SynC") %>%
  mutate(Wn = reg1_algprp$Wn)

reg1_algprp %>%
  ggplot(aes(x = Wn, y = C)) +
  geom_point(alpha = .5) +
  geom_line(data = fitC,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_C, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Synthesis C",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksSynC.png",scale = 3,dpi = 600)
# Syn D -------------------------------------------------------------------
peaks_D <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 1.543587139*Voigt(x = Wn,x0 = 1693.74571159172,sigma = 0.000119914131979676,gamma = 9.5)) %>%
  mutate(v2 = 0.411825379985445*Voigt(x = Wn,x0 = 1710.76953088495,sigma = 0.00102367485723181,gamma = 9.5)) %>%
  mutate(v3 = 20.7418291848059*Voigt(x = Wn,x0 = 1651.46478102515,sigma = 16.9595313785716,gamma = 9.5)) %>%
  mutate(v4 = 35.9373015425264*Voigt(x = Wn,x0 = 1606.27197710414,sigma = 31.1247181342153,gamma = 8)) %>%
  mutate(v5 = 1.91123139264748*Voigt(x = Wn,x0 = 1519.93496931972,sigma = 6.83883685773886,gamma = 6.5)) %>%
  mutate(v6 = 1.40873827589429*Voigt(x = Wn,x0 = 1631.39492783926,sigma = 9.96081283790272,gamma = 10)) %>%
  mutate(v7 = 8.84003665093371*Voigt(x = Wn,x0 = 1678.85112811467,sigma = 10.9490426379321,gamma = 9.5)) %>%
  mutate(v8 = 7.26800719877454*Voigt(x = Wn,x0 = 1544.54759317617,sigma = 13.4199946214937,gamma = 8.75)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:8,sep = "")))

fitD <- tmp %>%
  filter(spec == "SynD") %>%
  mutate(Wn = reg1_algprp$Wn)

reg1_algprp %>%
  ggplot(aes(x = Wn, y = D)) +
  geom_point(alpha = .5) +
  geom_line(data = fitD,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_D, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Synthesis D",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksSynD.png",scale = 3,dpi = 600)
# Syn E -------------------------------------------------------------------
peaks_E <- tibble(Wn = reg1_algprp$Wn) %>%
  mutate(v1 = 0.604216240782855*Voigt(x = Wn,x0 = 1690.98871526049,sigma = 0.100004848314223,gamma = 9.5)) %>%
  mutate(v2 = 1.85005472086391*Voigt(x = Wn,x0 = 1539.47377587001,sigma = 13.7163096181364,gamma = 5)) %>%
  mutate(v3 = 14.2986955267479*Voigt(x = Wn,x0 = 1666.24723613524,sigma = 20.1422051539919,gamma = 9.5)) %>%
  mutate(v4 = 0.196836846698023*Voigt(x = Wn,x0 = 1516.77831205695,sigma = 0.981453818199203,gamma = 5)) %>%
  mutate(v5 = 3.72979588722469*Voigt(x = Wn,x0 = 1638.75651839434,sigma = 11.5385160584745,gamma = 9.5)) %>%
  mutate(v6 = 0.691759786687348*Voigt(x = Wn,x0 = 1675.07981545606,sigma = 7.0654087874319,gamma = 11)) %>%
  mutate(v7 = 34.8647415451598*Voigt(x = Wn,x0 = 1610.80404798103,sigma = 35.0460002682502,gamma = 8.25)) %>%
  pivot_longer(cols = -Wn,names_to = "Bands",values_to = "Abs") %>%
  mutate(Bands = factor(Bands,levels = paste("v",1:8,sep = "")))

fitE <- tmp %>%
  filter(spec == "SynE") %>%
  mutate(Wn = reg1_algprp$Wn)

reg1_algprp %>%
  ggplot(aes(x = Wn, y = E)) +
  geom_point(alpha = .5) +
  geom_line(data = fitE,
            aes(x = Wn, y = Abs), color = "red",size = .5) +
  geom_area(data = peaks_E, aes(x = Wn, y = Abs, fill = Bands),
            position = "identity", alpha = .5) +
  scale_x_reverse(n.breaks = 10) +
  scale_fill_viridis(discrete = T,option = "A") +
  theme_bw() +
  labs(title = "Synthesis E",
       y = "Absorbance (u.a.)",
       x = "Wavenumber (cm-1)") +
  theme(text = element_text(family = "Perpetua"),
        plot.title = element_text(size = 25),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15,face = "bold"),axis.text = element_text(size = 15))

ggsave(filename = "AlgPRP/peaksSynE.png",scale = 3,dpi = 600)
