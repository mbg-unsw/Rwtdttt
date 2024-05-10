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
#' @param form an object of class "formula" (or one that can be coerced to that
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

  cpy <- as.data.table(data)

  obs.name <- all.vars(form)[1]

  # FIXME should we remove any observations that fall outside start ... end
  #       or just flag an error?
  # SG: two if statements added in the block of code that filters rows keeping min or max if dataset has multiple rows per subject


  # if (length(unique(cpy[, get(id)]))==dim(cpy)[1]) {
  #   data <- data
  # } else if (length(unique(data[, get(id)]))!=dim(data)[1] & reverse==FALSE) {
  #   # data[, new_date := first(get(obs.name)), by = get(id)]
  #   data <- data[, .SD[which.min(get(obs.name))], by = get(id)]
  # # } else data[, new_date := data.table::last(get(obs.name)), by = get(id)]
  # } else data <- data[, .SD[which.max(get(obs.name))], by = get(id)]

  if(!is.na(id)) {

    if(length(id)>1) {
      stop("id colname must be a single element")
    }

    if(!(id %in% names(cpy))) {
      stop(paste0("'", id, "'", "is not in data"))
    }


    if (length(unique(cpy[, get(id)]))!=dim(cpy)[1] & reverse==FALSE) {

      cpy <- cpy[get(obs.name)>=start & get(obs.name)<=end]
      cpy <- cpy[, .SD[which.min(get(obs.name))], by = get(id)]

    } else if (length(unique(cpy[, get(id)]))!=dim(cpy)[1] & reverse==TRUE) {

      cpy <- cpy[get(obs.name)>=start & get(obs.name)<=end]
      cpy <- cpy[, .SD[which.max(get(obs.name))], by = get(id)]

      # if the dataset provided has just one row per subject, and some dates (BUT not all of them) are out of the window defined by start and end
    } else if ((length(unique(cpy[, get(id)]))==dim(cpy)[1]) & (sum(cpy[, get(obs.name)]<start | cpy[, get(obs.name)]>end)!=0) & (sum(cpy[, get(obs.name)]<start | cpy[, get(obs.name)]>end)!=dim(cpy)[1])) {

      # keep only dates within the window
      cpy <- cpy[get(obs.name)>=start & get(obs.name)<=end]
      # and throw a warning
      warning("Some dates are out of the window defined by start and end. Keeping only rows within the window.")

      # if the dataset provided has just one row per subject, and ALL dates are out of the window defined by start and end
    } else if ((length(unique(cpy[, get(id)]))==dim(cpy)[1]) & sum(cpy[, get(obs.name)<start] | cpy[, get(obs.name)]>end)!=0 & sum(cpy[, get(obs.name)]<start | cpy[, get(obs.name)]>end)==dim(cpy)[1]) {

      # throw an error
      stop("All dates are out of the window defined by start and end")

    }
  }

  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # FIXME this code gives a false positive for model formulae like "y~x+dlnorm(...)"

  disttmp <- attr(terms(form, specials=c("dlnorm", "dweib", "dexp")), "specials")

  dist <- if(isTRUE(disttmp$dlnorm==2)) "lnorm" # need isTRUE() as value can be NULL
    else if(isTRUE(disttmp$dweib==2)) "weib"
    else if(isTRUE(disttmp$dexp==2)) "exp"
    else stop("model must use one of dlnorm, dweib or dexp")

  # parse 'parameters' to test if they match 'form', otherwise error

  # define column names in data
  data.names <- names(cpy)

  if(is.null(cpy) || (nrow(cpy)<1)) {
    stop("data must be non-empty")
  }

  if(is.null(obs.name)) {
    stop("obstime variable must be specified")
  }

  if(!(obs.name %in% data.names)) {
    stop(paste0("'", obs.name, "'", "is not in data"))
  }

  # Check if the user provided dates (discrete) or numbers (continuous)
  # XXXX do we need to record this in the fit object?
  # XXXX function definition currently includes no 'conttime' parameter

  # SG: should we let the user to supply the type of time variable (including conttime as a parameter of the function),
  # or should we just derive it within the function as it is right now?

  if(class(cpy[[obs.name]])=="Date" && class(start)=="Date" && class(end)=="Date")
    conttime <- 0
  else if(class(cpy[[obs.name]])=="numeric" && class(start)=="numeric" && class(end)=="numeric")
    conttime <- 1
  else stop(paste0("variables start, end and '", obs.name, "' must be either all of class Date or all of class numeric"))

  if(!conttime)
    delta <- as.double(end - start, units="days") + 1
  else
    delta <- end - start

  ntot <- nrow(cpy)

  # FIXME for now making a copy of 'data' and changing it

  if(reverse) {

    if(!conttime)
      cpy[, obs.time := 0.5 + as.double(end - get(obs.name), units="days")]
    else
      cpy[, obs.time := end - get(obs.name)]

    cpy <- cpy[,(obs.name):=NULL]
    setnames(cpy, "obs.time", obs.name)


  } else {

    if(!conttime)
      cpy[, obs.time := 0.5 + as.double(get(obs.name) - start, units="days")]
    else
      cpy[, obs.time := get(obs.name) - start]

    cpy <- cpy[,(obs.name):=NULL]
    setnames(cpy, "obs.time", obs.name)

  }


  # should be calculate the proportion in a different way according to reverse parameter?
  # [no, it's OK according to wtdttt.ado - Malcolm]
  nonprevend <- sum(cpy[, get(obs.name)] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot

  # do we want to generate a warning or an error?
  if(prp<0) warning("The proportion of incident users is a negative value")

  lpinit <- qlogis(prp) # XXXX user cannot supply lpinit

  # if the user hasn't supplied initial values, calculate some

  if(is.null(init)) {
    if(dist == "lnorm") {

      muinit <- mean(log(cpy[, get(obs.name)][cpy[, get(obs.name)]< 0.5 * delta]))
      lnsinit <- log(sd(log(cpy[, get(obs.name)][cpy[, get(obs.name)] < 0.5 * delta])))

      init <- list(logitp=lpinit, mu=muinit, lnsigma=lnsinit, delta=delta)

    } else if(dist == "weib") {

      lnbetainit <- log(1/(mean(cpy[, get(obs.name)][cpy[, get(obs.name)]< 0.5 * delta])))
      lnalphainit <- 0
      init <- list(logitp=lpinit, lnalpha=lnalphainit, lnbeta=lnbetainit, delta=delta)

    } else if(dist == "exp") {

      lnbetainit <- log(1/(mean(cpy[, get(obs.name)][cpy[, get(obs.name)]< 0.5 * delta])))
      init <- list(logitp=lpinit, lnbeta=lnbetainit, delta=delta)

    }
  } else
    init <- c(init, list(logitp=lpinit, delta=delta)) # merge our lpinit with user-supplied values

  # FIXME this is very crude
  form <- formula(gsub(")", ", delta)", deparse(form)))
  out <- mle2(form, parameters = parameters, fixed = list(delta = delta),
              start = init, data = cpy)

  out <- as(out, "wtd")
  out@delta <- delta
  out@dist <- dist
  out@depvar <- obs.name
  out@idvar <- if(is.na(id)) character(0) else id
  return(out)
}
