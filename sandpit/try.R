library(tidyverse)
library(bbmle)
library(haven)
library(tidyverse)


wtddat <- read_dta("d:/stovring/PharmacoEpid/R_wtdttt/wtddat_covar.dta")

wtddat %>%
  filter(packsize>=200)

# change to 150

wtddat %>%
  filter(packsize>=150)

wtddat <- mutate(wtddat,
                 wtdtimes = 1 - last_rxtime,
                 packcat = factor(packsize))
