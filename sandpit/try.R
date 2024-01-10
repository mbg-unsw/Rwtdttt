rm(list = ls())

# load data
df <- haven::read_dta("data/wtddat_dates.dta")
df <- as.data.table(df)

# create time variable
test <- create_time(data = df, event.date.colname = rx1time, start = as.Date('2014-01-01'))

# fit waiting time distribution
fit1 <- wtdttt(data = df,
               obstime ~ dlnorm(logitp, mu, lnsigma),
               event.date.colname = rx1time,
               event.time.colname = obstime,
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
               init = list(logitp = 1.65, mu = 4.23, lnsigma = -1.26),
               )

summary(fit1)

predict(fit1, family = "lnorm")
predict(fit1, family = "lnorm", type = "prob", distrx=c(2,100))

########################### RANDOM INEDX DATE

# by hand

# create sampling window and define a random index date for each individual
set.seed(84)

df <- df %>%
       mutate(r_index_date = sample(as.Date(as.Date("2014-01-01"):as.Date("2014-12-31")), nrow(df), replace=T),
              obstime = as.numeric(rx1time - r_index_date))

# remove dispensations not within the observation window
df <- df %>%
       filter(obstime>=0)

################

# calling the created function
test <- create_time_random(data = df, event.date.colname = rx1time, index.date.colname = r_index_date)
