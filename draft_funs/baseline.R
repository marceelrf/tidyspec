

  spec_bl_rollingBall <- function(.data,
                                   wn_col = "Wn",
                                   Wn_min,
                                   Wn_max,
                                   wm,
                                   ws,
                                   is_abs = TRUE) {

    mat <- .data[.data[[wn_col]] >= Wn_min & .data[[wn_col]] <= Wn_max,]
    if(is_abs){
      mat %>%
        select(-{{wn_col}}) %>%
        t() %>%
        baseline(method = "rollingBall",wm=wm, ws=ws) %>%
        pluck("baseline") %>%
        t() %>%
        as_tibble() %>%
        mutate({{wn_col}} := mat[[wn_col]]) %>%
        select({{wn_col}},where(is.numeric))
    } else {
      mat %>%
        spec_trans2abs(wn_col = {{wn_col}}) %>%
        select(-{{wn_col}}) %>%
        t() %>%
        baseline(method = "rollingBall",wm=wm, ws=ws) %>%
        pluck("baseline") %>%
        t() %>%
        as_tibble() %>%
        mutate({{wn_col}} := mat[[wn_col]]) %>%
        select({{wn_col}}, where(is.numeric))
    }

  }

alg %>%
  select(-Wn) %>%
  t() %>%
  baseline(method = "rollingBall",wm=67, ws=55) %>%
  pluck("baseline") %>%
  t() %>%
  as_tibble()


tmp1 <- spec_bl_rollingBall(alg_3,wn_col = "Wn",Wn_min = 1512.5,Wn_max = 1775,wm = 67,ws = 55,is_abs = F)
alg_3 %>%
  spec_trans2abs() %>%
  #filter(Wn < 1775, Wn > 1512.5) %>%
  #spec_smooth_sga() %>%
  #spec_blc_rollingBall(wn_col = "Wn",Wn_min = 1512.5,Wn_max = 1775,wm = 67,ws = 55,is_abs = F) %>%
  spec_smartplot() +
  geom_line(data = tmp1, aes(x= Wn, y = V1),inherit.aes = F, color = "yellow") +
  geom_line(data = tmp1, aes(x= Wn, y = V2),inherit.aes = F, color = "purple") +
  geom_line(data = tmp1, aes(x= Wn, y = V3),inherit.aes = F, color = "cyan")

# Baseline ----------------------------------------------------------------
library(recipes)
alg %>%
  spec_trans2abs() %>%
  spec_smooth_sga() %>%
  dplyr::filter(Wn >=1500,Wn <=1800) %>%
  recipe(formula = Wn ~ .,
       data = .) %>%
  step_bs(all_numeric_predictors(),
          degree = 1) %>%
  prep() %>%
  bake(NULL) %>%
  left_join(spec_trans2abs(alg), by = "Wn") %>%
  # select(Wn,where(is.numeric)) %>%
  smartplot()
