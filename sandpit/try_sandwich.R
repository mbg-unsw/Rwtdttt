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
               #id = "pid",
               start = 0, end = 1
)

summary(fit1)

fit1@idvar <- "pid" # XXXX nasty hack

Rwtdttt:::sand_vcov(fit1)
