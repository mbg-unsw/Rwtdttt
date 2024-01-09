#' The Lognormal distribution
#'
#' @param x vector of quantiles
#' @param logitp how to describe this?
#' @param mu mean
#' @param lnsigma log of standard deviation
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
    if(prob<0 | prob>1) stop("prob not between 0 and 1")

    sigma <- exp(lnsigma)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    if (log == FALSE) {
      d <- prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta
    } else {
      d <- log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta)
    }

    return(density=d)

    if(any(d<0 | d>1)) warning("density values not between 0 and 1")
}


#' The Weibull distribution
#'
#' @param x vector of quantiles
#' @param logitp how to describe this?
#' @param lnalpha log of alpha
#' @param lnbeta log of beta
#' @param log logical; if TRUE, probabilities p are given as log(p)
#'
#' @return
#' @export
#'
#' @examples
dweib <- function(x, logitp, lnalpha, lnbeta, log = FALSE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  if(prob<0 | prob>1) stop("prob not between 0 and 1")

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
#' @param x vector of quantiles
#' @param logitp how to describe this?
#' @param lnalpha log of alpha
#' @param lnbeta log of beta
#' @param log logical; if TRUE, probabilities p are given as log(p)
#'
#' @return
#' @export
#'
#' @examples
dexp <- function(x, logitp, lnbeta, log = FALSE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  if(prob<0 | prob>1) stop("prob not between 0 and 1")

  beta <- exp(lnbeta)

  if (log == FALSE) {
    d <- prob * exp(-((x*exp(lnbeta))) ) * exp(lnbeta) + (1-prob)/delta
  } else {
    d <- log(prob * exp(-((x*exp(lnbeta))) ) * exp(lnbeta) + (1-prob)/delta)
  }
  return(density=d)
}
