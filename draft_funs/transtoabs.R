alg %>%
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71) %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_naomit() %>%
  step_mutate_at(all_predictors(), fn = function(x) 2-log10(x)) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,where(is.numeric)) %>%
  dplyr::filter(!is.infinite(-Wn)) %>%
  smartplot()

alg %>%
  filter(is.na(Trans))
# Transmittance to Absorbance ---------------------------------------------
spec_trans2abs <- function(.data){
  require(recipes)

  .data %>%
    recipe(formula = Wn ~ .,
         data = .) %>%
    step_mutate_at(all_predictors(), fn = function(x) 2-log10(x)) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}

spec_abs2trans <- function(.data){
  require(recipes)

  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_mutate_at(all_predictors(), fn = function(x) 10^(2-x)) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}

