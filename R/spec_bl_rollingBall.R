spec_bl_rollingBall <- function(.data,
                                wn_col = "Wn",
                                Wn_min,
                                Wn_max,
                                wm,
                                ws,
                                is_abs = TRUE) {

  mat <- .data[.data[[wn_col]] >= Wn_min & .data[[wn_col]] <= Wn_max,]
  if(is_abs){
    mat %>%
      select(-{{wn_col}}) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("baseline") %>%
      t() %>%
      as_tibble() %>%
      mutate({{wn_col}} := mat[[wn_col]]) %>%
      select({{wn_col}},where(is.numeric))
  } else {
    mat %>%
      spec_trans2abs(wn_col = {{wn_col}}) %>%
      select(-{{wn_col}}) %>%
      t() %>%
      baseline(method = "rollingBall",wm=wm, ws=ws) %>%
      pluck("baseline") %>%
      t() %>%
      as_tibble() %>%
      mutate({{wn_col}} := mat[[wn_col]]) %>%
      select({{wn_col}}, where(is.numeric))
  }

}
