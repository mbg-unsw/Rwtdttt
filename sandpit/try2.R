# Quick test that new way of setting up delta works

library(data.table)
library(tidyverse)
library(haven)
library(bbmle)

# load data
df <- haven::read_dta("data/wtddat_dates.dta")

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               rx1time ~ dlnorm(logitp, mu, lnsigma),
               event.date.colname = rx1time,
               event.time.colname = rx1time,
               id.colname = "pid",
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
               )


summary(fit1)
