#' Derivative of WTD log-likelihood wrt omega (=logitp)

d1domega <- function(dfun, x, logitp, param1, param2 = 0, delta = 1) {
  prob <- exp(logitp) / (1 + exp(logitp))


  if (dfun == dlnorm) {
    mu <- param1
    sigma <- exp(param2)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    gfun <- pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean
  }

  if (dfun == dweib) {
    beta <- exp(param1)
    alpha <- exp(param2)

    gfun <- exp(-((x*exp(lnbeta))^exp(lnalpha)) -
                  lgamma(1+1/exp(lnalpha))) * exp(lnbeta)
  }

  if (dfun == dexp) {
    beta <- exp(param1)
    gfun <- exp(-((x*exp(lnbeta))) ) * exp(lnbeta)
  }


  d1 <- prob * (delta * gfun - 1) / (delta * gfun * exp(logitp) + 1)

  return(d1lomega = d1)
}

#' Derivative of WTD log-likelihood wrt param1 (and param2)

d1dtheta <- function(dfun, logitp, param1, param2 = 0, delta = 1) {
  prob <- exp(logitp) / (1 + exp(logitp))

  if (dfun == dlnorm) {
    mu <- param1
    # s is also known as lnsigma
    s <- param2
    sigma <- exp(param2)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    gfun <- pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean


    y <- exp(-s) * (log(x) - mu)

    d1FRDdmu <-(exp(-s) * dnorm(-y) - pnorm(-y)) / mean
    d1FRDds <- (exp(-s) * y * dnorm(-y) - exp(2 * s) * pnorm(-y)) / mean

    d1ldmu <- d1FRDmu / (gfun + exp(-logitp) + 1)
    d1lds < d1FRDs / (gfun + exp(-logitp) + 1)
    return(d1lmu = d1ldmu, d1lnsigma = d1lds)
  }


}
