wtdtttpredprob <- function(distrx) {
  
  mu <- m@fullcoef[2]
  lnsigma <- m@fullcoef[3]
  
  prttt <- pnorm(-(log(distrx)-mu)/exp(lnsigma))
  return(prttt)
}


################# PROVA

data_predprob <- read_dta("G:/Il mio Drive/Dottorato unipi/R functions/Stata files/lastRx_index.dta")

wtdtttpredprob(distrx = data_predprob$distlast)

plot(data_predprob$distlast, prttt, type = "l")
