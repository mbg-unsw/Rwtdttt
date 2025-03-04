# wtdttt - R functions and documentation

# make sure class definition is listed first in DESCRIPTION Collate:
#' @include wtd-class.R
NULL


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
#' The model formula `parameters` follows the pattern `list(alpha ~ "covariate", beta ~ "covariate", gamma ~ 1)`
#' with
#'
#' * `covariate`: the variable that is informative about the duration to the next prescription redemption and that will affect the estimate of the parameters of the model
#' In the pattern reported above the covariaste only affect alpha and beta, but not gamma (since 1 is supplied after ~)
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
#' @param preprocess logical; Pre-process the data to limit to one observation
#' per id. If id is omitted, defaults to F (default T).
#' @param subset an optional vector specifying a subset of observations to be
#' used in the fitting process. If the variable for which you want to create the
#' subset is a factor, it is necessary to use both double and single quotation marks
#' in the following way: i.e., subset = 'sex=="F"'
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
#' @importFrom methods as is
#' @importFrom stats terms na.pass sd qlogis formula as.formula model.frame qnorm
#' @importFrom data.table as.data.table setkeyv
#' @importFrom rlang enquo eval_tidy
#' @export
#'
#' @examples
#' # Fit the WTD with a lognormal distribution ----------------------------------------------------
#'
#' # load data
#' df <- haven::read_dta(system.file("extdata", "wtddat_dates.dta", package="Rwtdttt"))
#'
#' # fit the model
#' fit1 <- wtdttt(data = df,
#'                form = rx1time ~ dlnorm(logitp, mu, lnsigma),
#'                id = "pid",
#'                start = as.Date('2014-01-01'),
#'                end = as.Date('2014-12-31')
#' )
#'
#' # Fit a reverse WTD with covariates -------------------------------------------------------------
#'
#' # load data
#' df <- haven::read_dta(system.file("extdata", "wtddat_covar.dta", package="Rwtdttt"))
#'
#' # make packsize a factor
#' df$packsize <- as.factor(df$packsize)
#'
#' # fit the model
#' fit1 <- wtdttt(data = df,
#'                form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
#'                start = 0,
#'                end = 1,
#'                reverse = TRUE,
#'                parameters = list(logitp ~ packsize, mu ~ packsize, lnsigma ~ packsize)
#' )


wtdttt <- function(data, form, parameters=NULL, start=NA, end=NA, reverse=F, id=NA,
                   preprocess=T, subset=NULL, na.action=na.omit, init=NULL, control=NULL, ...) {


  if(is.null(data) || (nrow(data)<1)) {
    stop("data must be non-empty")
  }

  cpy <- as.data.table(data)


  # define column names in data
  data.names <- names(cpy)

  obs.name <- all.vars(form)[1]

  # 03/02/25
  covar.names <- unique(unlist(lapply(parameters, function(x) all.vars(x)[-1])))

  cpy <- na.action(cpy, cols = c(obs.name, covar.names))

  ##

  if(is.null(obs.name)) {
    stop("obstime variable must be specified in model formula")
  }

  if(!(obs.name %in% data.names)) {
    stop(paste0("'", obs.name, "'", "is not in data"))
  }

  if(is(cpy[[obs.name]], "Date") && is(start, "Date") && is(end, "Date"))
    conttime <- 0
  else if(is(cpy[[obs.name]], "numeric") && is(start, "numeric") && is(end, "numeric"))
    conttime <- 1
  else stop(paste0("variables start, end and '", obs.name, "' must be either all of class Date or all of class numeric"))


  if(!is.null(substitute(subset))) {

      rows <- enquo(subset)
      rows_val <- eval_tidy(rows, cpy)
      cpy <- cpy[rows_val ,]

    if(nrow(cpy)<1) {
      stop("data must be non-empty")
    }

   }


  cpy <- cpy[cpy[[obs.name]]>=start & cpy[[obs.name]]<=end]

  if(is.na(id)) {

    warning("The id variable was not provided so all data will be used, considering there is one row per subject")
    preprocess=F

  }

  if(preprocess) {

    if(length(id)>1) {
      stop("id colname must be a single element")
    }

    if(!(id %in% names(cpy))) {
      stop(paste0("'", id, "'", "is not in data"))
    }

    # XXXX probably no need to do this check
    if (nrow(unique(cpy[, id, with=F]))!=nrow(cpy)) {
      if(reverse==FALSE) {

        kc <- c(id, obs.name); setkeyv(cpy, kc) # XXXX consider using indexing
        cpy <- cpy[, .SD[1L], by = ids, env=list(ids=id)]

      } else  {

        kc <- c(id, obs.name); setkeyv(cpy, kc) # XXXX consider using indexing
        cpy <- cpy[, .SD[.N], by = ids, env=list(ids=id)]

        # XXXX this will never happen, out-of-range data were filtered out earlier
        # if the dataset provided has just one row per subject, and some dates (BUT not all of them) are out of the window defined by start and end
      }} else if ((sum(cpy[[obs.name]]<start | cpy[[obs.name]]>end)!=0) & (sum(cpy[[obs.name]]<start | cpy[[obs.name]]>end)!=nrow(cpy))) {

      # throw a warning
      warning("Some dates are out of the window defined by start and end. Keeping only rows within the window.")

      # if the dataset provided has just one row per subject, and ALL dates are out of the window defined by start and end
    } else if ((sum(cpy[[obs.name]]<start | cpy[[obs.name]]>end)!=0 & sum(cpy[[obs.name]]<start | cpy[[obs.name]]>end)==nrow(cpy))) {

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


  if(!conttime)
    delta <- as.double(end - start, units="days") + 1
  else
    delta <- end - start

  ntot <- nrow(cpy)


  if(reverse) {

    if(!conttime)
      cpy[, obs.time := 0.5 + as.double(end - cpy[[obs.name]], units="days")]
    else
      cpy[, obs.time := end - cpy[[obs.name]]]

    cpy <- cpy[,(obs.name):=NULL]
    setnames(cpy, "obs.time", obs.name)


  } else {

    if(!conttime)
      cpy[, obs.time := 0.5 + as.double(cpy[[obs.name]] - start, units="days")]
    else
      cpy[, obs.time := cpy[[obs.name]] - start]

    cpy <- cpy[,(obs.name):=NULL]
    setnames(cpy, "obs.time", obs.name)

  }


  nonprevend <- sum(cpy[, obs.name, with=F] > (delta * 2/3))
  prp <- 1 - 3 * nonprevend / ntot

  if(prp<0) warning("The proportion of incident users is a negative value")

  lpinit <- qlogis(prp)

  # if the user hasn't supplied initial values, calculate some

  if(is.null(init)) {
    if(dist == "lnorm") {

      muinit <- mean(log(cpy[[obs.name]][cpy[[obs.name]] < 0.5*delta]))
      lnsinit <- log(sd(log(cpy[[obs.name]][cpy[[obs.name]] < 0.5*delta])))

      init <- list(logitp=lpinit, mu=muinit, lnsigma=lnsinit, delta=delta)

    } else if(dist == "weib") {

      lnbetainit <- log(1/(mean(cpy[[obs.name]][cpy[[obs.name]] < 0.5*delta])))
      lnalphainit <- 0
      init <- list(logitp=lpinit, lnalpha=lnalphainit, lnbeta=lnbetainit, delta=delta)

    } else if(dist == "exp") {

      lnbetainit <- log(1/(mean(cpy[[obs.name]][cpy[[obs.name]] < 0.5*delta])))
      init <- list(logitp=lpinit, lnbeta=lnbetainit, delta=delta)

    }
  } else
    init <- c(init, list(logitp=lpinit, delta=delta)) # merge our lpinit with user-supplied values

  # FIXME this is very crude
  form <- formula(gsub(")", ", delta)", deparse(form)))
  out <- mle2(form, parameters = parameters, fixed = list(delta = delta),
              start = init, data = cpy)


  start <- as.character(start)
  end <- as.character(end)

  out <- as(out, "wtd")
  out@delta <- delta
  out@start <- start
  out@end <- end
  out@dist <- dist
  out@depvar <- obs.name
  out@idvar <- if(is.na(id)) character(0) else id
  out@isreverse <- reverse
  return(out)

}
