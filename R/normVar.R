normVar <- function(x){
  tmp1 <- x - mean(x)
  tmp2 <- tmp2/sd(x)

  return(tmp2)
}
