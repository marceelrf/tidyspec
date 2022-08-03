spec_smooth_sga <- function(.data, wn_col = "Wn", window = 15,forder = 4,degree = 0){
  require(recipes)
  require(signal)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))



  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_mutate_at(all_numeric_predictors(),
                   fn = function(x) sgolayfilt(x = x,
                                               p = forder,
                                               n = window,
                                               m = degree)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
