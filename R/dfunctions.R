#' The Lognormal distribution
#'
#' @param x vector of prescription redemption times
#' @param logitp log-odds of being a prevalent user
#' @param mu mean on log-scale
#' @param lnsigma log of standard deviation on log-scale
#' @param log logical; if TRUE, probabilities p are given as log(p).
#'
#' @return
#' @export
#'
#' @examples
#'
#'

delta <- 365

dlnorm <- function(x, logitp, mu, lnsigma, log = FALSE) {

    prob <- exp(logitp) / (1 + exp(logitp))
    sigma <- exp(lnsigma)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    if (log == FALSE) {
      d <- prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta
    } else {
      d <- log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta)
    }
    return(density=d)
}


#' The Weibull distribution
#'
#' @param x vector of prescription redemption times
#' @param logitp log-odds of being a prevalent user
#' @param lnalpha log of alpha (shape)
#' @param lnbeta log of beta (scale)
#' @param log logical; if TRUE, probabilities p are given as log(p)
#'
#' @return
#' @export
#'
#' @examples
dweib <- function(x, logitp, lnalpha, lnbeta, log = FALSE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  alpha <- exp(lnalpha)
  beta <- exp(lnbeta)

  if (log == FALSE) {
    d <- prob * exp(-((x*exp(lnbeta))^exp(lnalpha)) - lgamma(1+1/exp(lnalpha))) * exp(lnbeta) + (1-prob)/delta
  } else {
    d <- log(prob * exp(-((x*exp(lnbeta))^exp(lnalpha)) - lgamma(1+1/exp(lnalpha))) * exp(lnbeta) + (1-prob)/delta)
  }
  return(density=d)
}


#' The Exponential distribution
#'
#' @param x vector of prescription redemption times
#' @param logitp log-odds of being a prevalent user
#' @param lnbeta log of beta (scale)
#' @param log logical; if TRUE, probabilities p are given as log(p)
#'
#' @return
#' @export
#'
#' @examples
dexp <- function(x, logitp, lnbeta, log = FALSE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  beta <- exp(lnbeta)

  if (log == FALSE) {
    d <- prob * exp(-((x*exp(lnbeta))) ) * exp(lnbeta) + (1-prob)/delta
  } else {
    d <- log(prob * exp(-((x*exp(lnbeta))) ) * exp(lnbeta) + (1-prob)/delta)
  }
  return(density=d)
}
