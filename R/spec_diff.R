spec_diff <- function(.data, degree=1){
  require(recipes)
  require(timetk)


  if(degree == 0) {
    .data
  } else {

    .data %>%
      recipe(formula = Wn ~ .,
             data = .) %>%
      step_diff(all_numeric_predictors(),difference = degree,lag = 1) %>%
      prep() %>%
      bake(NULL) %>%
      select(Wn,where(is.numeric)) %>%
      select(Wn, starts_with("diff"))
  }
}
