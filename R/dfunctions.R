# Lognormal

#' Title
#'
#' @param x
#' @param logitp
#' @param mu
#' @param lnsigma
#' @param log
#'
#' @return
#' @export
#'
#' @examples
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

# Weibull

#' Title
#'
#' @param x
#' @param logitp
#' @param lnalpha
#' @param lnbeta
#' @param log
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

# Exponential

#' Title
#'
#' @param x
#' @param logitp
#' @param lnalpha
#' @param lnbeta
#' @param log
#'
#' @return
#' @export
#'
#' @examples
dexp <- function(x, logitp, lnbeta, log = FALSE) {

  prob <- exp(logitp) / (1 + exp(logitp))
  beta <- exp(lnbeta)

  if (log == FALSE) {
    d <- prob * exp(-((x*exp(lnbeta))) - lgamma(1)) * exp(lnbeta) + (1-prob)/delta
  } else {
    d <- log(prob * exp(-((x*exp(lnbeta))) - lgamma(1)) * exp(lnbeta) + (1-prob)/delta)
  }
  return(density=d)
}
