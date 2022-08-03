spec_smooth_avg <- function(.data, wn_col= "Wn", window = 15, degree = 2){
  require(recipes)
  require(timetk)
  require(rlang)
  require(dplyr)


  fmla <- as.formula(paste({{wn_col}}," ~ .",sep = ""))

  .data %>%
    recipe(formula = fmla,
           data = .) %>%
    step_smooth(all_numeric_predictors(),
                period = window,
                degree = degree) %>%
    prep() %>%
    bake(NULL) %>%
    select({{wn_col}},where(is.numeric))
}
