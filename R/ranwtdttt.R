# ranwtdttt - R functions and documentation

#' Fit Waiting Time Distribution with random index times
#'
#' `ranwtdttt()` estimates maximum likelihood estimates for parametric Waiting Time Distribution (WTD)
#' based on observed prescription redemptions with adjustment for covariates
#' using one or more random index times for each individual. Reports estimates
#' of prevalence fraction and specified percentile of inter-arrival density
#' together with regression coefficients.
#'
#' @param form an object of class "formula" (or one that can be coered to that
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
ranwtdttt <- function(form, parameters=NULL, data, id, start, end, reverse=F,
                      nsamp=1, subset, na.action=na.pass, init, control=NULL, ...) {

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
