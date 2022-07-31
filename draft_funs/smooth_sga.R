# Test --------------------------------------------------------------------
?signal::sgolayfilt()
?pracma::savgol()
alg %>%
  recipe(formula = Wn ~ .,
         data = .) %>%
  step_mutate_at(all_numeric_predictors(),
                 fn = function(x) sgolayfilt(x = x,
                                             p = 4,
                                             n = 15)) %>%
  prep() %>%
  bake(NULL) %>%
  select(Wn,where(is.numeric)) %>%
  ggplot(aes(x = Wn, y = Trans)) +
  geom_line()



# Function ----------------------------------------------------------------

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


# Testing -----------------------------------------------------------------
alg_3 %>%
  spec_smooth_sga(window = 21) %>%
  spec_trans2abs() %>%
  left_join(spec_trans2abs(alg_3),by = c("Wn"),suffix = c("_filtered","_prev")) %>%
  pivot_longer(cols = -1,names_to = "Vars",values_to = "Vals") %>%
  dplyr::filter(Wn  <=1760, Wn >= 1000) %>%
  mutate(Type = case_when(str_detect(Vars,pattern = "filter") ~ "Filtered",
                          TRUE ~ "Prev")) %>%
  ggplot(aes(Wn,Vals,col = Vars)) +
  geom_line(size = .5) +
  facet_wrap(~Type)

