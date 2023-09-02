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
