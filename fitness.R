source("testPIP.R")
fitness <- function(SP, template){
  TD <- sqrt(sum((SP$T - template$T)^2))
  AD <- sqrt(sum((SP$A - template$A)^2))
  w <- 0.4
  DM <- w*AD+(1-w)*TD
  return(DM)
}


