# wtdttt - R functions and documentation

# Libraries are loaded via "Imports" in DESCRIPTION
# library(bbmle)
# library(class)
#' @importClassesFrom bbmle mle2
#'

# Register a 'wtd' class, inheriting from 'mle2'
# FIXME perhaps this belongs in a separate file

#' @exportClass wtd
setClass("wtd", contains="mle2", slots=c(delta="numeric", dist="character",
                                         depvar="character"))

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
create_time <- function(event.date.colname, data, start, time.name, ...) {

  event.date.colname <- deparse(substitute(event.date.colname))
  # data[,obstime := as.numeric(0.5 + get(event.date.colname) - start)]
  data[,(time.name) := as.numeric(0.5 + get(event.date.colname) - start)]

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
#' @importFrom bbmle mle2
#' @importFrom methods as
#' @importFrom stats terms na.pass sd qlogis
#' @export
#'
#' @examples
wtdttt <- function(data, form, covariates=NULL, id.colname=NA, event.date.colname=NA, event.time.colname=NA, start=NA, end=NA, reverse=F,
                   subset=NA, na.action=na.pass, init=NULL, control=NULL, ...) {

  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # FIXME this code gives a false positive for model formulae like "y~x+dlnorm(...)"

  # disttmp <- attr(terms(form, specials=c("dlnorm", "dweib", "dexp")), "specials")

  # dist <- if(deparse(substitute(dlnorm))=="lnorm") # need isTRUE() as value can be NULL
  #   else if(deparse(substitute(dlnorm))=="weib"
  #   else if(deparse(substitute(dlnorm))=="exp"
  #   else stop("model must use one of dlnorm, dweib or dexp")

  # parse 'parameters' to test if they match 'form', otherwise error

  # test if start, end are dates, error if a mix of types
  # test if outcome variable is a date
  # if start, end are dates and outcome is not, error
  # FIXME code currently assumes data are always dates and discrete not continuous

  # do continuity correction if required
  # FIXME function definition currently includes no 'conttime' parameter

  # id.colname <- deparse(substitute(id.colname))

  # define column names in data
  data.names <- names(data)

  if(is.null(data) || (nrow(data)<1)) {
    stop("data must be non-empty")
  }

  # XXXX should id be optional? not required unless doing random index times I think
  if(is.null(id.colname)) {
    stop("id colname must be non-empty")
  }

  if(length(id.colname)>1) {
    stop("id colname must be a single element")
  }

  if(!(id.colname %in% data.names)) {
    stop("id colname is not in data")
  }

  # XXXX we could use model.response() to get the response var name from 'form' instead
  # computing starting values
  # XXXX need Sabrina to explain how this is meant to work -- discuss
  # event.date.colname <- deparse(substitute(event.date.colname))
  # event.time.colname <- deparse(substitute(event.time.colname))

  delta <- as.double(end - start, units="days") + 1
  ntot <- nrow(data)

  # FIXME for now making a copy of 'data' and changing it
  # XXXX hard-coding continuity correction for now


  # cpy <- data
  # if(reverse)
  #   data[, get(event.time.colname)] <-
  #    # 0.5 + as.double(end - data[[get(event.time.colname)]], units="days")
  #   0.5 + as.double(end - data[, get(event.time.colname)], units="days")
  # else
  #   data[, get(event.time.colname)] <-
  #     # 0.5 + as.double(data[[get(event.time.colname)]] - start, units="days")
  #   0.5 + as.double(data[, get(event.time.colname)] - start, units="days")


  nonprevend <- sum(data[, get(event.time.colname)] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot

  print(prp)

  # do we want to generate a warning or an error?
  if(prp<0) warning("The proportion of incident users is a negative value")

  lpinit <- qlogis(prp) # XXXX user cannot supply lpinit

  # if the user hasn't supplied initial values, calculate some

  if(is.null(init)) {
    # if(deparse(substitute(form)) == "lnorm") {
    if(form == "lnorm") {

      # muinit <- mean(log(obstime[obstime < 0.5 * delta]))
      # lnsinit <- log(sd(log(obstime[obstime < 0.5 * delta])))

      # XXXX event.time.colname is a char vector, can use without 'get()' for indexing the data.frame
      muinit <- mean(log(data[, get(event.time.colname)][data[, get(event.time.colname)]< 0.5 * delta]))
      lnsinit <- log(sd(log(data[, get(event.time.colname)][data[, get(event.time.colname)] < 0.5 * delta])))

      init <- list(logitp=lpinit, mu=muinit, lnsigma=lnsinit, delta=delta)

    # } else if(deparse(substitute(form)) == "weib") {
    } else if(form == "weib") {

      lnbetainit <- log(1/(mean(data[, get(event.time.colname)][data[, get(event.time.colname)]< 0.5 * delta])))
      lnalphainit <- 0
      init <- list(logitp=lpinit, lnalpha=lnalphainit, lnbeta=lnbetainit, delta=delta)

    # } else if(deparse(substitute(form)) == "exp") {
    } else if(form == "exp") {

      lnbetainit <- log(1/(mean(data[, get(event.time.colname)][data[, get(event.time.colname)]< 0.5 * delta])))
      init <- list(logitp=lpinit, lnbeta=lnbetainit, delta=delta)

    }
  } else
    init <- c(init, list(logitp=lpinit, delta=delta)) # merge our lpinit with user-supplied values

  # FIXME this is very crude
  formula_c <-  paste0(event.time.colname, "dlnorm(logitp, mu, lnsigma, delta)", sep = " ~ ")
  # form <- formula(gsub(")", ", delta)", deparse(form)))
  # browser()
  # out <- mle2(formula_c, parameters = covariates, fixed = list(delta = delta),
  #             start = init, data = data)
  out <- mle2(formula_c, parameters = covariates,
              start = init, data = data)

  out <- as(out, "wtd") # need to store more things in the output object e.g. delta
  out@delta <- delta
  # out@dist <- dist
  out@dist <- deparse(substitute(form))
  out@depvar <- event.time.colname
  return(out)
}
