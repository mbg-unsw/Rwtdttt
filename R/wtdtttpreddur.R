# Case with iadmean==FALSE
#           disttype==lnorm

wtdtttpreddur <- function(iadpercentile) {
  
   mu <- m@fullcoef[2]
   lnsigma <- m@fullcoef[3]
   
   rxdur <- exp(qnorm(iadpercentile)*exp(lnsigma)+mu)
  
}