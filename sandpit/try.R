rm(list = ls())

# load data
df <- read_dta("data/wtddat_dates.dta")
df <- as.data.table(df)

# create time variable
test <- create_time(data = df, event.date.colname = rx1time, start = as.Date('2014-01-01'))

# fit waiting time distribution
fit1 <- wtdttt(obstime ~ dlnorm(logitp, mu, lnsigma),
               data = df,
               event.date.colname = rx1time,
               event.time.colname = obstime,
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
               init = list(logitp = 1.65, mu = 4.23, lnsigma = -1.26),
               )

summary(fit1)

predict(fit1, family = "lnorm")
predict(fit1, family = "lnorm", type = "prob", distrx=c(2,100))
