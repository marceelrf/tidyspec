fn1<- function(.data,wn_col,
         Wn_min,
         Wn_max,
         wm,
         ws,
         is_abs = TRUE) {

  if(is_abs){
    .data %>%
      dplyr::filter({{wn_col}} >=Wn_min,{{wn_col}} <=Wn_max) %>%
      select(-{{wn_col}}) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("corrected") %>%
      t() %>%
      as_tibble() %>%
      mutate({{wn_col}} = .data %>% dplyr::filter({{wn_col}} >=Wn_min,{{wn_col}} <=Wn_max) %>% pull({{wn_col}})) %>%
      select({{wn_col}},where(is.numeric))
  } else {
    .data %>%
      dplyr::filter({{wn_col}} >=Wn_min,{{wn_col}} <=Wn_max) %>%
      spec_trans2abs() %>%
      select(-{{wn_col}}) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("corrected") %>%
      t() %>%
      as_tibble() %>%
      mutate({{wn_col}} = .data %>% dplyr::filter({{wn_col}} >=Wn_min,{{wn_col}} <=Wn_max) %>% pull({{wn_col}})) %>%
      select({{wn_col}},where(is.numeric))
  }

}

