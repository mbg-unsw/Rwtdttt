library(tidyverse)
library(bbmle)
library(haven)


wtddat <- read_dta("d:/stovring/PharmacoEpid/R_wtdttt/wtddat_covar.dta")

wtddat <- mutate(wtddat,
                 wtdtimes = 1 - last_rxtime,
                 packcat = factor(packsize))
