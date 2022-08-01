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
