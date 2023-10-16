# Rwtdttt
# Combine Henrik, Malcolm & Sabrina's progress so far
# Create a "wtd" model class and use it to set up a 'predict' method

# Test it

library(tidyverse)
library(bbmle)
library(haven)

x <- read_dta("data/wtddat_dates.dta")

# XXXX a bunch of preprocessing that should actually be
#      implemented in wtdttt()

start <- as.Date('2014-01-01'); end<-as.Date('2014-12-31')
x$obstime <- 0.5 + as.double(x$rx1time - start, units="days")

delta <- as.double(end - start, units="days") + 1
ntot <- nrow(x)
nonprevend <- sum(x$obstime > (delta * 2/3))
prp <- 1 - 3 * nonprevend / ntot
lpinit <- qlogis(prp)

muinit <- mean(log(x$obstime[x$obstime < 0.5 * delta]))
lnsinit <- log(sd(log(x$obstime[x$obstime < 0.5 * delta])))

# ###############

x.w <- wtdttt(obstime ~ dlnorm(logitp, mu, lnsigma), data=x,
	start=as.Date('2014-01-01'), end=as.Date('2014-12-31'), reverse=F,
	init=list(logitp=lpinit, mu = muinit, lnsigma = lnsinit))

summary(x.w)

# Predict duration at default quantile=0.8
predict(x.w)

# Predict probability
predict(x.w, type="prob", distrx=c(10,100))

# Diagnostic plot
plot(x.w)


