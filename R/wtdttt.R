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
wtdttt <- function(form, parameters=NULL, data, event.date.colname, event.time.colname, start, end, reverse=F,
                   subset, na.action=na.pass, init, control=NULL, ...) {

  # computing starting values
  event.date.colname <- deparse(substitute(event.date.colname))
  event.time.colname <- deparse(substitute(event.time.colname))

  # obstime <- 0.5 + as.double(data[[event.date.colname]] - start, units="days")
  delta <- as.double(end - start, units="days") + 1
  ntot <- nrow(data)
  # nonprevend <- sum(obstime > (delta * 2/3))
  nonprevend <- sum(data[, get(event.time.colname)] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot
  lpinit <- qlogis(prp)

  # muinit <- mean(log(obstime[obstime < 0.5 * delta]))
  # lnsinit <- log(sd(log(obstime[obstime < 0.5 * delta])))

  muinit <- mean(log(data[, get(event.time.colname)][data[, get(event.time.colname)]< 0.5 * delta]))
  lnsinit <- log(sd(log(data[, get(event.time.colname)][data[, get(event.time.colname)] < 0.5 * delta])))

  # lnbetainit <- log(1/(mean(obstime[obstime < 0.5 * delta])))
  # lnalphainit <- 0

  out <- mle2(form, parameters = parameters,
              start = init, data = data)

  as(out, "wtd")
}