spec_norm_minmax <- function(.data,wn_col = "Wn", min, max){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_range(all_numeric_predictors(),min = min,max = max) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
