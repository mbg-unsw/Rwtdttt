# wtdttt - R functions and documentation

# make sure class definition is listed first in DESCRIPTION Collate:
#' @include wtd-class.R
NULL

# create_time <- function(event.date.colname, data, start, ...) {
#
#   event.date.colname <- deparse(substitute(event.date.colname))
#   data[,obstime := as.numeric(0.5 + get(event.date.colname) - start)]
#
# }
#
# create_time_random <- function(event.date.colname, data, ...) {
#
#   event.date.colname <- deparse(substitute(event.date.colname))
#   # index.date.colname <- deparse(substitute(index.date.colname))
#
#   data[,r_index_date := sample(as.Date(as.Date("2014-01-01"):as.Date("2014-12-31")), nrow(data), replace = T)]
#   data[,obstime := as.numeric(0.5 + get(event.date.colname) - r_index_date)]
#
# }

#########################################################

#' Fit Waiting Time Distribution
#'
#' Estimates the maximum likelihood estimate for a parametric Waiting Time
#' Distribution (WTD) based on observed prescription redemptions with
#' adjustment for covariates. Reports estimates of prevalence fraction
#' and specified percentile of inter-arrival density together with
#' regression coefficients.
#'
#' @section Model formula:
#' The model formula `form` follows the pattern `obstime ~ dist(alpha, beta, gamma)`
#' with
#'
#' * `obstime`: the redemption time variable (date or real number)
#' * `dist`: the parametric distribution for the forward or backward recurrence
#' density (FRD/BRD), which must be `dexp()`, `dweib()` or `dlnorm()`
#' i.e named after their corresponding interarrival density (IAD).
#'
#' @section Data format:
#' The WTD is fit to the first prescription redemption of each
#' individual within an observation window (ordinary WTD), or the last
#' (reverse WTD), respectively.
#'
#' You may prepare the data to this format, or optionally specify the name of
#' an `id` variable to select the first or last redemption automatically.
#'
#' If the redemption time data are of type date, a continuity correction will be
#' applied automatically.
#'
#' @param form an object of class "formula" (or one that can be coered to that
#' class): a symbolic description of the model to be fitted. The details of the
#' model specification are given under 'Details'
#' @param parameters optional model formulae for distribution parameters
#' @param data an optional data frame, list or environment (or object coercible
#' by as.data.frame to a data frame) containing the variables in the model. If
#' not found in data, the variables are taken from environment(formula),
#' typically the environment from which wtdttt is called.
#' @param start start of observation window (date or real number)
#' @param end end of observation window (date or real number)
#' @param reverse logical; Fit the reverse waiting time distribution (default F).
#' @param id name of the id variable (optional)
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
#' @importFrom stats terms na.pass sd qlogis formula
#' @export
#'
#' @examples
wtdttt <- function(data, form, parameters=NULL, start=NA, end=NA, reverse=F, id=NA,
                   subset=NA, na.action=na.pass, init=NULL, control=NULL, ...) {

  obs.name <- all.vars(form)[1]

  if (length(unique(data[, get(id)]))==dim(data)[1]) {
    data <- data
  } else if (length(unique(data[, get(id)]))!=dim(data)[1] & reverse==FALSE) {
    # data[, new_date := first(get(obs.name)), by = get(id)]
    data <- data[, .SD[which.min(get(obs.name))], by = get(id)]
  # } else data[, new_date := data.table::last(get(obs.name)), by = get(id)]
  } else data <- data[, .SD[which.max(get(obs.name))], by = get(id)]


  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # FIXME this code gives a false positive for model formulae like "y~x+dlnorm(...)"

  disttmp <- attr(terms(form, specials=c("dlnorm", "dweib", "dexp")), "specials")

  dist <- if(isTRUE(disttmp$dlnorm==2)) "lnorm" # need isTRUE() as value can be NULL
    else if(isTRUE(disttmp$dweib==2)) "weib"
    else if(isTRUE(disttmp$dexp==2)) "exp"
    else stop("model must use one of dlnorm, dweib or dexp")

  # parse 'parameters' to test if they match 'form', otherwise error

  # test if start, end are dates, error if a mix of types
  # test if outcome variable is a date
  # if start, end are dates and outcome is not, error
  # FIXME code currently assumes data are always dates and discrete not continuous

  # do continuity correction if required
  # FIXME function definition currently includes no 'conttime' parameter

  # define column names in data
  data.names <- names(data)

  if(is.null(data) || (nrow(data)<1)) {
    stop("data must be non-empty")
  }


  if(is.null(obs.name)) {
    stop("obstime variable must be specified")
  }

  # if(length(id.colname)>1) {
  #   stop("id colname must be a single element")
  # }

  if(!(obs.name %in% data.names)) {
    stop(paste0("'", obs.name, "'", "is not in data"))
  }

  # FIXME check the data type of the obs.name and only convert if a date
  delta <- as.double(end - start, units="days") + 1
  ntot <- nrow(data)

  # FIXME for now making a copy of 'data' and changing it
  # XXXX hard-coding continuity correction for now, should not apply if data
  # are real numbers and not dates

  cpy <- data
  if(reverse)
    cpy[, obs.name] <-
     0.5 + as.double(end - cpy[[obs.name]], units="days")
  else
    cpy[, obs.name] <-
      0.5 + as.double(cpy[[obs.name]] - start, units="days")

  nonprevend <- sum(cpy[, obs.name] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot

  # do we want to generate a warning or an error?
  if(prp<0) warning("The proportion of incident users is a negative value")

  lpinit <- qlogis(prp) # XXXX user cannot supply lpinit

  # if the user hasn't supplied initial values, calculate some

  if(is.null(init)) {
    if(dist == "lnorm") {

      # muinit <- mean(log(obstime[obstime < 0.5 * delta]))
      # lnsinit <- log(sd(log(obstime[obstime < 0.5 * delta])))

      # XXXX event.time.colname is a char vector, can use without 'get()' for indexing the data.frame
      muinit <- mean(log(cpy[, obs.name][cpy[, obs.name]< 0.5 * delta]))
      lnsinit <- log(sd(log(cpy[, obs.name][cpy[, obs.name] < 0.5 * delta])))

      init <- list(logitp=lpinit, mu=muinit, lnsigma=lnsinit, delta=delta)

    } else if(dist == "weib") {

      lnbetainit <- log(1/(mean(cpy[, obs.name][cpy[, obs.name]< 0.5 * delta])))
      lnalphainit <- 0
      init <- list(logitp=lpinit, lnalpha=lnalphainit, lnbeta=lnbetainit, delta=delta)

    } else if(dist == "exp") {

      lnbetainit <- log(1/(mean(cpy[, obs.name][cpy[, obs.name]< 0.5 * delta])))
      init <- list(logitp=lpinit, lnbeta=lnbetainit, delta=delta)

    }
  } else
    init <- c(init, list(logitp=lpinit, delta=delta)) # merge our lpinit with user-supplied values

  # FIXME this is very crude
  form <- formula(gsub(")", ", delta)", deparse(form)))
  out <- mle2(form, parameters = parameters, fixed = list(delta = delta),
              start = init, data = cpy)

  out <- as(out, "wtd") # need to store more things in the output object e.g. delta
  out@delta <- delta
  out@dist <- dist
  out@depvar <- obs.name
  # out@idvar <- id
  return(out)
}
