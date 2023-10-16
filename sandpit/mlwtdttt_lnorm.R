mlwtdttt_lnorm <- function(logitp, mu, lnsigma) {
  
  a <- -sum(log(plogis(logitp) * pnorm(-(log(obstime)-mu)/exp(lnsigma))/exp(mu + exp(2*lnsigma)/2)+(plogis(-logitp))/delta))
  return(a)
  
}
