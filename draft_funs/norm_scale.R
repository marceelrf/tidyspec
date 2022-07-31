library(recipes)
alg %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_scale(all_numeric_predictors()) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,Trans) %>%
  left_join(alg, by = "Wn") %>%
  smartplot()

#norm
alg %>%
  # mutate(spec = Trans*1.1,
  #        spec2 = Trans*.71) %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_normalize(all_numeric_predictors()) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,Trans) %>%
  left_join(alg, by = "Wn") %>%
  smartplot()


#0 to 1 - funciona mas é melhor testar com novos dados



alg %>% # para 1 spectro
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71) %>%
  spec_trans2abs() %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_naomit() %>%
  step_range(all_numeric_predictors()) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,where(is.numeric)) %>%
  smartplot()

alg %>%
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71) %>%
  spec_trans2abs() %>%
  pivot_longer(-1,names_to = "Spectra",values_to = "Vals") %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_naomit() %>%
  step_range(all_numeric_predictors()) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,where(is.numeric),"Spectra") %>%
  pivot_wider(names_from = Spectra,values_from = Vals) %>%
  smartplot()

# Norm01 function ---------------------------------------------------------
# Não esta funcionando
spec_norm_01 <- function(tbl){
  require(dplyr)
  require(tidyr)
  require(recipes)

  tbm1 <- tbl %>%
    pivot_longer(-1,names_to = "Spectra",values_to = "Vals") %>%
    summarise(max=max(Vals)) %>%
    pull(max)

  tbl %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_naomit() %>%
    step_mutate_at(all_numeric_predictors(),
                   fn = function(x) x-min(x)) %>%
    step_mutate_at(all_numeric_predictors(),
                   fn = function(x) x/tbm1) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric))

}
