spec_norm_01 <- function(.data) {

  maxVal <- .data %>%
    spec_trans2abs() %>%
    mutate(across(.cols = -Wn,
                  .fns = function(x) x-min(x)))

  maxVal %>%
    mutate(across(.cols = -Wn,
                  .fns = function(x) x/max(maxVal[,-1])))
}
