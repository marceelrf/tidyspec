library(timetk)
alg_3 <- alg %>%
  filter(Wn >=1500,Wn <=2000) %>%
  mutate(spec = Trans*1.1,
         spec2 = Trans*.71)
alg_3 %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_smooth(all_numeric_predictors(),
              period = 15,) %>%
  prep() %>%
  bake(NULL) %>%
  left_join(alg_3, by = "Wn",suffix = c("_smoothed","_prev")) %>%
  select(Wn,where(is.numeric)) %>%
  smartplot(alpha = 1)


# Smooth Average ----------------------------------------------------------


spec_smooth_avg <- function(.data, window = 15, degree = 2){
  require(recipes)
  require(timetk)

  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_smooth(all_numeric_predictors(),
                period = window,
                degree = degree) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric))
}
