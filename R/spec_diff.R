spec_diff <- function(.data,wn_col = "Wn", degree=1){
  require(recipes)
  require(timetk)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))


  if(degree == 0) {
    .data
  } else {

    .data %>%
      recipe(formula = fmla,
             data = .) %>%
      step_diff(all_numeric_predictors(),difference = degree,lag = 1) %>%
      prep() %>%
      bake(NULL) %>%
      select({{wn_col}},where(is.numeric)) %>%
      select({{wn_col}}, starts_with("diff"))
  }
}
