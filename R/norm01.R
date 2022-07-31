norm01 <- function(x){
  tmp1 <- x - min(x)
  tmp2 <- tmp1/max(tmp1)

  return(tmp2)
}
