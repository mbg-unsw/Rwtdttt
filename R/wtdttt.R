# wtdttt - R functions and documentation

# Libraries are loaded via "Imports" in DESCRIPTION
# library(bbmle)
# library(class)
#' @importClassesFrom bbmle mle2
#'

# Register a 'wtd' class, inheriting from 'mle2'
# FIXME perhaps this belongs in a separate file

setClass("wtd", contains="mle2")

#' Create time variable
#'
#' @param event.date.colname
#' @param data
#' @param start
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
create_time <- function(event.date.colname, data, start, ...) {

  event.date.colname <- deparse(substitute(event.date.colname))
  data[,obstime := as.numeric(0.5 + get(event.date.colname) - start)]

}

create_time_random <- function(event.date.colname, data, ...) {

  event.date.colname <- deparse(substitute(event.date.colname))
  # index.date.colname <- deparse(substitute(index.date.colname))

  data[,r_index_date := sample(as.Date(as.Date("2014-01-01"):as.Date("2014-12-31")), nrow(data), replace = T)]
  data[,obstime := as.numeric(0.5 + get(event.date.colname) - r_index_date)]

}

#########################################################

# Trying to implement pre-processing code in the function

# Is it acceptable to modify the R landscape, creating a new variable within the data.frame or even a numerical vector?
# It is fine for all numerical objects (delta, ntot, etc.) because we create them within the function, use them, but not return them. They will never come up in the environment
# PROBLEM with obstime

#' Fit Waiting Time Distribution
#'
#' Estimates the maximum likelihood estimate for a parametric Waiting Time
#' Distribution (WTD) based on observed prescription redemptions with
#' adjustment for covariates. Reports estimates of prevalence fraction
#' and specified percentile of inter-arrival density together with
#' regression coefficients.
#'
#' @param form an object of class "formula" (or one that can be coered to that
#' class): a symbolic description of the model to be fitted. The details of the
#' model specification are given under 'Details'
#' @param parameters model formulae for distribution parameters
#' @param data an optional data frame, list or environment (or object coercible
#' by as.data.frame to a data frame) containing the variables in the model. If
#' not found in data, the variables are taken from environment(formula),
#' typically the environment from which wtdttt is called.
#' @param event.date.colname
#' @param event.time.colname
#' @param start start of observation window
#' @param end end of observation window
#' @param reverse logical; Fit the reverse waiting time distribution.
#' @param subset an optional vector specifying a subset of observations to be
#' used in the fitting process.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. The default is set by the na.action setting of options, and is
#' na.fail if that is unset. The 'factory-fresh' default is na.omit. Another
#' possible value is NULL, no action. Value na.exclude can be useful.
#' @param init starting values for the parameters.
#' @param control a list of parameters for controlling the fitting process.
#' @param ... further arguments passed to other methods.
#'
#' @return wtdttt returns an object of class "wtd" inheriting from "mle".
#' @export
#'
#' @examples
wtdttt <- function(data, form, parameters=NULL, id.colname=NA, event.date.colname=NA, event.time.colname=NA, start=NA, end=NA, reverse=F,
                   subset=NA, na.action=na.pass, init=NULL, control=NULL, ...) {

  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # FIXME this code gives a false positive for models like "y~x+dlnorm(...)"

  disttmp <- attr(terms(form, specials=c("dlnorm", "dweib", "dexp")), "specials")

  dist <- if(isTRUE(disttmp$dlnorm==2)) "lnorm" # need isTRUE() as value can be NULL
    else if(isTRUE(disttmp$dweib==2)) "weib"
    else if(isTRUE(disttmp$dexp==2)) "exp"
    else stop("model must use one of dlnorm, dweib or dexp")

  # parse 'parameters' to test if they match 'form', otherwise error

  # test if start, end are dates, error if a mix of types
  # test if outcome variable is a date
  # if start, end are dates and outcome is not, error

  # do continuity correction if required

  # id.colname <- deparse(substitute(id.colname))

  # define column names in data
  data.names <- names(data)

  if(is.null(data) || (nrow(data)<1)) {
    stop("data must be non-empty")
  }

  if(is.null(id.colname)) {
    stop("id colname must be non-empty")
  }

  if(length(id.colname)>1) {
    stop("id colname must be a single element")
  }

  if(!(id.colname %in% data.names)) {
    stop("id colname is not in data")
  }

  # computing starting values
  event.date.colname <- deparse(substitute(event.date.colname))
  event.time.colname <- deparse(substitute(event.time.colname))

  # obstime <- 0.5 + as.double(data[[event.date.colname]] - start, units="days")
  delta <- as.double(end - start, units="days") + 1
  ntot <- nrow(data)
  # nonprevend <- sum(obstime > (delta * 2/3))
  nonprevend <- sum(data[, get(event.time.colname)] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot

  # do we want to generate a warning or an error?
  if(prp<0) warning("The proportion of incident users is a negative value")


  # if the user hasn't supplied initial values, calculate some

  if(is.null(init)) {
    lpinit <- qlogis(prp)
    if(dist == "lnorm") {

      # muinit <- mean(log(obstime[obstime < 0.5 * delta]))
      # lnsinit <- log(sd(log(obstime[obstime < 0.5 * delta])))

      muinit <- mean(log(data[, get(event.time.colname)][data[, get(event.time.colname)]< 0.5 * delta]))
      lnsinit <- log(sd(log(data[, get(event.time.colname)][data[, get(event.time.colname)] < 0.5 * delta])))

      init <- list(logitp=lpinit, mu=muinit, lnsigma=lnsinit)
    } else if(dist == "weib") {

      # lnbetainit <- log(1/(mean(obstime[obstime < 0.5 * delta])))
      # lnalphainit <- 0
      # init <- list(logitp=lpinit, lnalpha=lnalphainit, lnbeta=lnbetainit)

    } else if(dist == "dexp") {
      # init <- list(logitp=lpinit, lnbeta=lnbetainit)
    }
  }

  # Redefining density functions to use the computed delta to scale
  # the function to be a proper density
  dlnorm <- function(x, logitp, mu, lnsigma, log = TRUE)
    dlnorm(x, logitp, mu, lnsigma, delta = delta, log)

  dweib <- function(x, logitp, lnalpha, lnbeta, log = TRUE)
    dweib(x, logitp, lnalpha, lnbeta, delta = delta, log)

  dexp <- function(x, logitp, lnbeta, log = TRUE)
    dexp(x, logitp, lnbeta, delta = delta, log)

  out <- mle2(form, parameters = parameters,
              start = init, data = data)

  as(out, "wtd") # need to store more things in the output object e.g. delta
}
