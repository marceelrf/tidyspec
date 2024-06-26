#' Remove baseline drift from a spectral data set
#'
#' @param .data A data frame with columns Wn and one or more spectral data
#' @param Wn_min Minimum Wn value to consider
#' @param Wn_max Maximum Wn value to consider
#' @param wm Window size for moving average calculation
#' @param ws Window size for baseline determination
#' @param is_abs A logical indicating if the input data is in absorbance units
#' @return A data frame with Wn and the corrected spectral data
#' @export
#' @examples
#' spec_blc_rollingBall(data, 2000, 4000, wm=15, ws=30)
#'

spec_blc_rollingBall <- function(.data,
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
      pluck("corrected") %>%
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
      pluck("corrected") %>%
      t() %>%
      as_tibble() %>%
      mutate({{wn_col}} := mat[[wn_col]]) %>%
      select({{wn_col}},where(is.numeric))
  }

}
