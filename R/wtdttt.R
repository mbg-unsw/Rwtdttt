if (!require("zoo")) install.packages("zoo")
library(zoo)
if (!require("haven")) install.packages("haven")
library(haven)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

wtdttt <- function(time, start, end) {
  
  # continuity correction for discrete data
  tstart <- as.numeric(start) - 0.5
  tend <- as.numeric(end) + 0.5
  delta <- tend - tstart
  
  # if(reverse==TRUE) {
  #   obstime <- as.numeric(as.Date(tend)-as.Date(time)) }
  #   else {
  #   obstime <- as.numeric(as.Date(time)-as.Date(tstart))
    # }
  
    time <- as.numeric(time)
    obstime <- as.numeric(as.Date(time)-as.Date(tstart))
  
  # initial values
  ntot <- length(time)
  nonprevend <- sum(obstime > (delta*2/3))
  prp <- 1 - 3*nonprevend/ntot
  lpinit <- log((prp)/(1-prp)) # logit
  
  # df <- df %>%
  #        mutate(logtime = case_when(
  #          obstime<delta*0.5 ~ log(obstime),
  #          TRUE ~ obstime))
  
  muinit <- mean(log(obstime[which(obstime<delta*0.5)]))
  lnsinit <- log(sd(log(obstime[which(obstime<delta*0.5)])))
  
  # start: initial parameters to be used as a starting point for the optimization
  mle2(mlwtdttt_lnorm, start = list(mu=muinit, lnsigma=lnsinit, logitp=lpinit))
  
}

########### 

data_wtd <- read_dta("G:/Il mio Drive/Dottorato unipi/R functions/Stata files/wtddat_dates.dta")

m <- wtdttt(time = data_wtd$rx1time, start = as.Date('2014-01-01'), end = as.Date('2014-12-31'))

summary(m)

