# Rwtdttt - R functions and documentation

# Combine Henrik, Malcolm & Sabrina's progress so far
# Create a "wtd" model class and use it to set up a 'predict' method
# This is a proof of concept to show how we can overload 'predict()' etc

# Libraries are loaded via "Imports" in DESCRIPTION
# library(bbmle)
# library(class)
#' @importClassesFrom bbmle mle2
#'

# Register a 'wtd' class, inheriting from 'mle2'

setClass("wtd", contains="mle2")

# create a new environment for package global variables, e.g. "delta"
# https://stackoverflow.com/questions/12598242/global-variables-in-packages-in-r

wtdttt.env <- new.env(parent=emptyenv())
wtdttt.env$delta <- 1

#' The Lognormal Distribution
#'
#' @param x vector of quantiles
#' @param logitp how to describe this?
#' @param mu mean
#' @param lnsigma log of standard deviation
#' @param log logical; if TRUE, probabilities p are given as log(p).
#'
#' @return
#'
#' @examples
dlnorm <- function(x, logitp, mu, lnsigma, log = FALSE) {

    prob <- exp(logitp) / (1 + exp(logitp))
    sigma <- exp(lnsigma)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    if (log == FALSE) {
      prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / wtdttt.env$delta
    } else {
      log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / wtdttt.env$delta)
    }

}

#' Fit Waiting Time Distribution
#'
#' `wtdttt()` estimates the maximum likelihood estimate for a parametric Waiting Time
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
wtdttt <- function(form, parameters=NULL, data, start, end, reverse=F,
	subset, na.action=na.pass, init, control=NULL, ...) {

  # parse 'form' to determine the distribution in use and test if it
  # is a supported one, otherwise error

  # parse 'parameters' to test if they match 'form', otherwise error

  # test if start, end are dates, error if a mix of types
  # test if outcome variable is a date
  # if start, end are dates and outcome is not, error

  # do continuity correction if required

  # depending on distribution in use, calculate initial values (if not supplied)

	out <- mle2(form, parameters = parameters,
             start = init, data = data)

	as(out, "wtd") # XXXX may need to store more things in the output obj
}

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


# XXXX only handling the lnorm case for now
# Copied Sabrina's code, is there a normalising factor/logitp missing?

#' Make WTD predictions
#'
#' Make predictions based on an estimated parametric Waiting Time Distribution
#' (WTD) model, either the probability of a person still being in treatment or
#' the duration of observed prescription redemptions.
#'
#' @param wtd a fitted object of class inheriting from "wtd"
#'
#' @return A vector of predictions
#' @export
setMethod("predict", "wtd",
	function(object, newdata=NULL, type="dur", distrx=NULL, quantile=0.8,
		se.fit=FALSE, na.action=na.pass, ...) {
	if(type=="dur") {
		# Case with iadmean==FALSE

		   mu <- object@fullcoef[2]
		   lnsigma <- object@fullcoef[3]

		   out <- exp(qnorm(quantile)*exp(lnsigma)+mu)

	} else if(type=="prob") {

		  mu <- object@fullcoef[2]
		  lnsigma <- object@fullcoef[3]

		  out <- pnorm(-(log(distrx)-mu)/exp(lnsigma))
	}
	out
})

#' Make WTD diagnostic plots
#'
#' Make diagnostic plots showing the fit of an estimated parametric Waiting Time
#' Distribution (WTD) with respect to the observed histogram of prescription
#' redemptions.
#'
#' @param wtd wtd object, typically result of wtdttt
#' @export
setMethod("plot", "wtd",
	function(object, x, y, ...) {

	   logitp <- object@fullcoef[1]
	   mu <- object@fullcoef[2]
	   lnsigma <- object@fullcoef[3]

	h <- hist(object@data$obstime, freq=F)
        d <- h$breaks[2] - h$breaks[1]
	curve(dlnorm(x, logitp, mu, lnsigma),
		from=0.1, to=max(object@data$obstime), add=T)
})

