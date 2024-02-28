# Quick test that new way of setting up delta works

library(data.table)
library(tidyverse)
library(haven)
library(bbmle)

# load data
df <- haven::read_dta("data/wtddat_dates.dta")

# create time variable
df <- as.data.table(df)

df <- create_time(data = df,event.date.colname = rx1time, time.name = "pippotime", start = as.Date('2014-01-01'))

df <- as.data.frame(df)

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               # rx1time ~ dlnorm(logitp, mu, lnsigma),
               form = dlnorm,
               event.date.colname = "rx1time",
               event.time.colname = "pippotime",
               id.colname = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31')
               )

summary(fit1)

plot(fit1)

# repeat with Weibull
fit2 <- wtdttt(data = df,
               rx1time ~ dweib(logitp, lnalpha, lnbeta),
               event.date.colname = rx1time,
               event.time.colname = rx1time,
               id.colname = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit2)
plot(fit2)

# ... and with Exponential
fit3 <- wtdttt(data = df,
               rx1time ~ dexp(logitp, lnbeta),
               event.date.colname = rx1time,
               event.time.colname = rx1time,
               id.colname = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
)


summary(fit3)

plot(fit3)

