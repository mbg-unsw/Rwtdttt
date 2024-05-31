# proof of concept sandwich estimation

library(haven)
library(bbmle)
library(numDeriv)


tmp <- read_dta("inst/extdata/score_ex.dta")

tmp$packcat <- factor(tmp$packsize)

delta <- 1

dlnorm2 <- function(x, logitp, mu, lnsigma, log = TRUE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  sigma <- exp(lnsigma)
  mean <- exp(mu + exp(2 * lnsigma)/2)

  if (log == FALSE) {

    prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta

  } else {

    log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta)

  }

}


fit1 <- mle2(rxtime ~ dlnorm2(logitp, mu, lnsigma),
             parameters = list(logitp ~ packcat, mu ~ packcat, lnsigma ~ packcat),
             start = list(logitp = 0, mu = log(delta/5), lnsigma = 0),
             data = tmp)

summary(fit1)

score_mat <- t(sapply(1:nrow(tmp),

                      function(i) {

                        logl <- function(x) {

                          mm <- model.matrix(~packcat, data=tmp)[i,]

                          dlnorm2(as.numeric(tmp[i, "rxtime"]),

                                  logitp=t(x[1:2]) %*% mm,

                                  mu=t(x[3:4]) %*% mm,

                                  lnsigma=t(x[5:6]) %*% mm,

                                  log=T)

                        }

                        grad(logl, fit1@coef)

                      }

))

colnames(score_mat) <- names(fit1@coef)

bread <- fit1@vcov

applyTapplySum <- function(X,index) apply(X, 2, function(col) tapply(col, index, sum))

psi <- applyTapplySum(as.matrix(score_mat), tmp$pid) # sum by cluster

meat <- crossprod(as.matrix(psi))

rownames(meat) <- colnames(meat) <- colnames(psi)

sand <- (NROW(unique(tmp$pid))/(NROW(unique(tmp$pid))-1)) * (bread %*% meat %*% bread)

sand
