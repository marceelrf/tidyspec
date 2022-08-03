spec_abs2trans <- function(.data, wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))
  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_mutate_at(all_predictors(), fn = function(x) 10^(2-x)) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric)) %>%
    filter_all(all_vars(!is.infinite(.)))
}
