spec_norm_var <- function(.data){
  require(recipes)


  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_scale(all_numeric_predictors()) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric))
}
