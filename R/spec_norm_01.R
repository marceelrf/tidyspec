spec_norm_01 <- function(.data){
  require(recipes)


  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_range(all_numeric_predictors()) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric))
}
