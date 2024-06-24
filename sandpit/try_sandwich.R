# proof of concept sandwich estimation

library(bbmle)
library(Rwtdttt)
library(haven)
library(data.table)
library(readxl)

# load data
# test data for sandwich from Henrik

tmp <- haven::read_dta(system.file("extdata", "score_ex.dta", package="Rwtdttt"))

tmp$packcat <- factor(tmp$packsize)

cl <- tmp[order(tmp$pid),]

fit1 <- wtdttt(data = cl,
               rxtime ~ dlnorm(logitp, mu, lnsigma),
               parameters = list(logitp ~ packcat, mu ~ packcat, lnsigma ~ packcat),
               #id = "pid", # do this to stop wtdttt() from filtering the data
               start = 0, end = 1
)

summary(fit1)

fit1@idvar <- "pid" # XXXX nasty hack to add the ids back in again for the sandwich

Rwtdttt:::sand_vcov(fit1)

# Equivalent Stata output

# . matrix list Dcl
#
# symmetric Dcl[6,6]
# logitp:     logitp:         mu:         mu:    lnsigma:    lnsigma:
#   ipack       _cons       ipack       _cons       ipack       _cons
# logitp:ipack   .61015454
# logitp:_cons   -.2340662    .2340662
# mu:ipack   .01835992  -.00759435   .04215239
# mu:_cons  -.00759435   .00759435    -.032557     .032557
# lnsigma:ipack    .0696015    -.045143  -.03004152   .02833014   .12762532
# lnsigma:_cons    -.045143     .045143   .02833014  -.02833014  -.07785109   .07785109

