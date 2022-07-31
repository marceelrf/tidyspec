alg %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_diff(all_numeric_predictors(),difference = 1:2) %>%
  prep() %>%
  bake(NULL) %>%
  # left_join(alg, by = "Wn",suffix = c("_diff","_prev")) %>%
  select(Wn,where(is.numeric)) %>%
  smartplot(alpha = 1)

spec_diff <- function(.data, degree=1){
  require(recipes)
  require(timetk)

  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_diff(all_numeric_predictors(),difference = degree) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric)) %>%
    select(Wn, starts_with("diff"))

}

alg %>%
  dplyr::filter(Wn >=1500,Wn <=1775) %>%
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71) %>%
  spec_trans2abs() %>%
  spec_smooth_sga(forder = 4,window = 51) %>%
  #spec_smooth_avg() %>%
  #spec_diff(degree = 1) %>%
  smartplot()
