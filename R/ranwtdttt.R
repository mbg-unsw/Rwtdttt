# ranwtdttt - R functions and documentation

#' @include wtd-class.R wtdttt.R
NULL

#' Fit Waiting Time Distribution with random index times
#'
#' `ranwtdttt()` estimates maximum likelihood estimates for parametric Waiting Time Distribution (WTD)
#' based on observed prescription redemptions with adjustment for covariates
#' using one or more random index times for each individual. It reports estimates
#' of prevalence fraction and specified percentile of inter-arrival density
#' together with regression coefficients.
#'
#' @param form an object of class "formula" (or one that can be coerced to that
#' class): a symbolic description of the model to be fitted. The details of the
#' model specification are given under 'Details'
#' @param parameters model formulae for distribution parameters
#' @param data an optional data frame, list or environment (or object coercible
#' by as.data.frame to a data frame) containing the variables in the model. If
#' not found in data, the variables are taken from environment(formula),
#' typically the environment from which wtdttt is called.
#' @param id the name of the variable that identifies distinct individuals
#' @param start start of observation window
#' @param end end of observation window
#' @param reverse logical; Fit the reverse waiting time distribution.
#' @param nsamp number of samples to take.
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
#' @importFrom data.table data.table setDT := .N .SD as.data.table setnames
#' @export
ranwtdttt <- function(form, parameters=NULL, data, id, start, end, reverse=F,
                      nsamp=4, subset, na.action=na.pass, init, control=NULL, ...) {

  data_init <- copy(data)

  # (to be modified, just to try) initializing an empty dataframe
  tmp <- data.table(pid = character(),
                    rxdate = as.Date(as.character()),
                    indda = as.Date(as.character()),
                    rxshift = as.Date(as.character()))

  # for loop to implement the multiple random index date
  for (i in 1:nsamp) {

  set.seed(84)

  obs.name <- all.vars(form)[1]

  delta <- as.numeric(end - start)

  if (!reverse) {

    data <- setDT(data)[, indda := sample(as.Date(as.Date(start):as.Date(end)), .N, replace=TRUE)][get(obs.name) >= indda & get(obs.name) <= indda + delta,][, .SD[which.min(get(obs.name))], by = id][, rxshift := get(obs.name) - (indda-start)]

    # browser()

  } else {

    # adatto come sopra (reverse=F)
    setDT(data)[, indda := sample(as.Date(as.Date(start):as.Date(end)), .N, replace=TRUE)][get(obs.name) <= indda & get(obs.name) >= indda - delta][, .SD[which.max(get(obs.name))], by = get(id)][, rxshift := get(obs.name) + end - indda]

  }

  # bind multiple copies of dataframe
  tmp <- rbind(tmp, data)

}

  # newform <- if()

  out <- wtdttt(rxshift ~ dlnorm(logitp, mu, lnsigma), parameters = parameters, start=start, end=end, id = id, data=tmp) # check row 103 in wtdttt: if(!is.na(id))

  return(out)

  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # parse 'parameters' to test if they match 'form', otherwise error

  # test if start, end are dates, error if a mix of types
  # test if outcome variable is a date
  # if start, end are dates and outcome is not, error
  # test if nsamp is an integer >=1

  # repeat nsamp times:
  #   generate a random start date for each id sample window
  #   extract dispense times within sample window for each id
  #   add to temporary analysis data set

  # call wtdttt using temporary data set

}
