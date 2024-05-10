# Quick test

library(bbmle)
library(Rwtdttt)
library(haven)
library(data.table)

# load data
# Dataset contains one observation per person, first dispensing for 2014
df <- haven::read_dta(system.file("extdata", "wtddat_dates.dta", package="Rwtdttt"))

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               rx1time ~ dlnorm(logitp, mu, lnsigma),
               id = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31')
               )


summary(fit1)

plot(fit1)

predict(fit1, type = "prob", distrx = df$rx1time)

# repeat with Weibull
# can omit the "id" parameter as the data are pre-processed

fit2 <- wtdttt(data = df,
               rx1time ~ dweib(logitp, lnalpha, lnbeta),
                start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit2)
plot(fit2)

predict(fit2, type=="prob")

# ... and with Exponential
fit3 <- wtdttt(data = df,
               rx1time ~ dexp(logitp, lnbeta),
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit3)

plot(fit3)

# Compare results with those obtained from wtdttt_ex.do in Stata ----

## 1) Open the example dataset (discrete time), ordinary WTD analysis with event dates ----
df <- haven::read_dta(system.file("extdata", "wtddat_dates.dta", package="Rwtdttt"))

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               rx1time ~ dlnorm(logitp, mu, lnsigma),
               id = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31')
)


summary(fit1)

## 2) Open the example dataset (continuous time), reverse WTD analysis with covariates ----
df <- haven::read_dta(system.file("extdata", "wtddat_covar.dta", package="Rwtdttt"))

### fit waiting time distribution (exp) ----
fit3 <- wtdttt(data = df,
               last_rxtime ~ dexp(logitp, lnbeta),
               start = 0, end = 1, reverse = T,
)


summary(fit3)

### fit waiting time distribution (log-normal) ----
fit1 <- wtdttt(data = df,
               last_rxtime ~ dlnorm(logitp, mu, lnsigma),
               # id = "pid",
               start = 0, end = 1, reverse = T
)

summary(fit1)

### fit waiting time distribution (weibull) ----
fit2 <- wtdttt(data = df,
               last_rxtime ~ dweib(logitp, lnalpha, lnbeta),
               start = 0, end = 1, reverse = T
)

summary(fit2)

### Use covariate in all three parameter equations ----

# parameters = list(logitp ~ packcat, mu ~ packcat, lnsigma ~ 1)
fit1 <- wtdttt(data = df,
               last_rxtime ~ dlnorm(logitp, mu, lnsigma),
               start = 0, end = 1, reverse = T, parameters = list(logitp ~ packsize, mu ~ packsize, lnsigma ~ packsize)
)

summary(fit1)


### Since covariate appears to have little influence on the lnsigma parameter, we estimate a model where number of pills only affect median parameter (mu) and the prevalence (logitp)
fit1 <- wtdttt(data = df,
               last_rxtime ~ dlnorm(logitp, mu, lnsigma),
               start = 0, end = 1, reverse = T, parameters = list(logitp ~ "packsize", mu ~ "packsize", lnsigma ~ 1)
)

summary(fit1)

## 3) A small example showing how treatment probability can be predicted based on the distance between index dates and date of last prescription redemption, while taking covariates (here: pack size) into account. The last fitted WTD is used for this prediction.
df <- haven::read_dta(system.file("extdata", "lastRx_index.dta", package="Rwtdttt"))

predict(fit1, type = "prob", distrx = df$distlast)


## 4) Random index date
df <- haven::read_dta(system.file("extdata", "ranwtddat_discdates.dta", package="Rwtdttt"))

fit_r <- ranwtdttt(data = df,
                   rxdate ~ dlnorm(logitp, mu, lnsigma), # change to shift date
                   id = "pid",
                   start = as.Date('2014-01-01'),
                   end = as.Date('2014-12-31')
)

summary(fit_r)


# Try on Jesper fake data (period: 02/01/95 - 31/08/22; drugs: clopidogrel, human insulin, metformin) ----

# load data
df <- haven::read_dta(system.file("extdata", "drugpakud.dta", package="Rwtdttt"))

# filter clopidogrel dispensations
df <- df %>%
       filter(atc=="B01AC04")

# fit waiting time distribution
fit4 <- wtdttt(data = df,
               rxdate ~ dlnorm(logitp, mu, lnsigma),
               id = "id",
               start = as.Date('2015-01-01'), end = as.Date('2015-12-31')
)


summary(fit4)

plot(fit4)

predict(fit4)
