spec_norm_var <- function(.data,wn_col = "Wn"){
  require(recipes)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))


  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_scale(all_numeric_predictors()) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
