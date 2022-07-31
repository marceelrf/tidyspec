baseline::






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
