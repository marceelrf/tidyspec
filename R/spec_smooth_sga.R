spec_smooth_sga <- function(.data, window = 15,forder = 4,degree = 0){
  require(recipes)
  require(signal)

  .data %>%
    recipe(formula = Wn ~ .,
           data = .) %>%
    step_mutate_at(all_numeric_predictors(),
                   fn = function(x) sgolayfilt(x = x,
                                               p = forder,
                                               n = window,
                                               m = degree)) %>%
    prep() %>%
    bake(NULL) %>%
    select(Wn,where(is.numeric))
}
