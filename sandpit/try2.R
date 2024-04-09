# Quick test

library(bbmle)
library(Rwtdttt)
library(haven)
library(data.table)

# load data
df <- haven::read_dta(system.file("extdata", "wtddat_dates.dta", package="Rwtdttt"))

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               rx1time ~ dlnorm(logitp, mu, lnsigma),
               id = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31')
               )


summary(fit1)

plot(fit1)

predict(fit1)

# repeat with Weibull
fit2 <- wtdttt(data = df,
               rx1time ~ dweib(logitp, lnalpha, lnbeta),
                start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit2)
plot(fit2)

# ... and with Exponential
fit3 <- wtdttt(data = df,
               rx1time ~ dexp(logitp, lnbeta),
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit3)

plot(fit3)

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
