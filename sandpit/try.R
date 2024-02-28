rm(list = ls())

library(data.table)
library(tidyverse)
library(haven)
library(bbmle)

# load data
df <- haven::read_dta("data/wtddat_dates.dta")
df <- as.data.table(df)

# create time variable
df_2 <- create_time(data = df, event.date.colname = rx1time, start = as.Date('2014-01-01'))


# fit waiting time distribution
fit1 <- wtdttt(data = df_2,
               obstime ~ dlnorm(logitp, mu, lnsigma),
               event.date.colname = rx1time,
               event.time.colname = rx1time,
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
               # init = list(mu = 4.23, lnsigma = -1.26),
               id.colname = "pid"
               )

###### ERROR
# if the user provides the init argument, the following error is thrown:
# Error in minuslogl(logitp = 1.65, logitp = 1.61280168302535, mu = 4.23,  :
# formal argument "logitp" matched by multiple actual arguments
#
# SOLVED removing lpinit as element of the list argument 'init'
#
# N.B. fit1@fullcoef includes the estimates of three parameters (logitp, mu, lnsigma) if init values are NOT provided by the user
#      fit2@fullcoef includes the estimates of two parameters (mu, lnsigma) if init values ARE provided by the user
#
# commented Henrik rows in wtdttt on redefinition of density functions
# run again external dlnorm function in dfunctions.R (with delta=365 in the definition of the function)

#############

summary(fit1)

predict(fit1, family = "lnorm")
predict(fit1, family = "lnorm", type = "prob", distrx=c(2,100))

plot(fit1)

########################### RANDOM INEDX DATE

# by hand

# create sampling window and define a random index date for each individual
set.seed(84)

######### if reverse==F

df_r <- df %>%
       mutate(r_index_date = sample(as.Date(as.Date("2014-01-01"):as.Date("2014-12-31")), nrow(df), replace=T),
              obstime = as.numeric(rx1time - r_index_date))

# remove dispensations not within the observation window
df_sel <- df_r %>%
       filter(obstime>=0)

# keep only the first dispensation, by id
df_f <- df_sel %>%
            group_by(pid) %>%
            slice_min(rx1time)

######### if reverse==T

df_r <- df %>%
  mutate(r_index_date = sample(as.Date(as.Date("2014-01-01"):as.Date("2014-12-31")), nrow(df), replace=T),
         obstime = as.numeric(rx1time - r_index_date))

# remove dispensations not within the observation window
df_sel <- df_r %>%
  filter(obstime>=0)

# keep only the first dispensation, by id
df_f <- df_sel %>%
  group_by(pid) %>%
  slice_min(rx1time)

########## ERROR
# i) if init values provided:
# Error in minuslogl(logitp = 1.76, logitp = 1.76026080216868, mu = 4.23,  :
# formal argument "logitp" matched by multiple actual arguments
#
# --> i) SOLVED: revoming lpinit from the init values provided
#
# ii) if not provided:
# Error in optim(par = c(logitp = 1.76026080216868, mu = -Inf, lnsigma = NaN :
#                          valore non finito passato da optim
###########

# calling the created function
df_sel <- create_time_random(data = df, event.date.colname = rx1time)

###########

# fit waiting time distribution
fit2 <- wtdttt(data = df_sel,
               obstime ~ dlnorm(logitp, mu, lnsigma),
               event.date.colname = rx1time,
               event.time.colname = obstime,
               start = as.Date('2014-01-01'), end = as.Date('2014-12-31'),
               init = list(mu = 4.23, lnsigma = -1.26)
)

summary(fit2)

predict(fit2, family = "lnorm")
predict(fit2, family = "lnorm", type = "prob", distrx=c(2,100))

plot(fit2)

################


