library(tidyverse)
library(bbmle)
library(haven)


wtddat <- read_dta("data/wtddat_covar.dta")

wtddat <- mutate(wtddat,
       wtdtimes = 1 - last_rxtime,
       packcat = factor(packsize))

delta <- 1

dwtdttt <- function(x, logitp, mu, lnsigma, log = FALSE)
{
  prob <- exp(logitp) / (1 + exp(logitp))
  sigma <- exp(lnsigma)
  mean <- exp(mu + exp(2 * lnsigma)/2)
  if (log == FALSE) {
    prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean
      + (1 - prob) / delta
  }
  else {
    log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean
        + (1 - prob) / delta)
  }
}

## Testing the density function - does it make sense?
wtddat$wtddens <- dwtdttt(wtddat$wtdtimes,
                          logitp = 1, mu = log(.15), lnsigma = -0.4, log = FALSE)
ggplot(data = wtddat) +
  geom_point(mapping = aes(x=wtdtimes, y=wtddens))


## Covariates can now be included (SIC!!!)
fit1 <- mle2(wtdtimes ~ dwtdttt(logitp, mu, lnsigma),
             parameters = list(logitp ~ packcat, mu ~ packcat, lnsigma ~ 1),
             start = list(logitp = 0, mu = log(delta/5), lnsigma = 0),
             data = wtddat)
summary(fit1)

##################################

wtddat <- read_dta("data/wtddat_covar.dta")

wtddat <- mutate(wtddat,
                 wtdtimes = 1 - last_rxtime,
                 packcat = factor(packsize))

delta <- 1

dwtdttt <- function(x, logitp, par1, par2, family = "lnorm") {

 if (family=="lnorm") {
    prob <- exp(logitp) / (1 + exp(logitp))
    sigma <- exp(par2)
    mean <- exp(par1 + exp(2 * par2)/2)

    prob * pnorm(log(x), par1, sigma, lower.tail = FALSE) / mean
    + (1 - prob) / delta

 } else if (family=="weibull") {
            prob <- exp(logitp) / (1 + exp(logitp))
            alpha <- exp(par1)
            beta <- exp(par2)

            log(prob * exp(-((x*exp(par2))^exp(par1)) - lgamma(1+1/exp(par1))) * exp(par2) + (1-prob)/delta)

 } else if (family=="exponential") {
            prob <- exp(logitp) / (1 + exp(logitp))
            alpha <- par1^0
            beta <- exp(par2)

            log(prob * exp(-((x*exp(par2))^alpha) - lgamma(1+1/alpha)) * exp(par2) + (1-prob)/delta)

 }

}

## Testing the density function - does it make sense?
wtddat$wtddens <- dwtdttt(wtddat$wtdtimes,
                          logitp = 1, par1 = 1, par2 = 0.2, family = "exponential")
ggplot(data = wtddat) +
  geom_point(mapping = aes(x=wtdtimes, y=wtddens))
