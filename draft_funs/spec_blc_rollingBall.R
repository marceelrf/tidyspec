spec_blc_rollingBall <- function(.data,
                                 Wn_min,
                                 Wn_max,
                                 wm,
                                 ws,
                                 is_abs = TRUE) {

  if(is_abs){
    .data %>%
      dplyr::filter(Wn >=Wn_min,Wn <=Wn_max) %>%
      select(-Wn) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("corrected") %>%
      t() %>%
      as_tibble() %>%
      mutate(Wn = alg %>% dplyr::filter(Wn >=Wn_min,Wn <=Wn_max) %>% pull(Wn)) %>%
      select(Wn,where(is.numeric))
  } else {
    .data %>%
      dplyr::filter(Wn >=Wn_min,Wn <=Wn_max) %>%
      spec_trans2abs() %>%
      select(-Wn) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("corrected") %>%
      t() %>%
      as_tibble() %>%
      mutate(Wn = alg %>% dplyr::filter(Wn >=Wn_min,Wn <=Wn_max) %>% pull(Wn)) %>%
      select(Wn,where(is.numeric))
  }

}
