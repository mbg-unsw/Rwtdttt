# predictions using wtd class

# make sure class definition is listed first in DESCRIPTION Collate:
#' @include wtd-class.R
NULL

#' Make WTD predictions
#'
#' Make predictions based on an estimated parametric Waiting Time Distribution
#' (WTD) model, either the probability of a person still being in treatment or
#' the duration of observed prescription redemptions.
#'
#' @param object a fitted object of class inheriting from "wtd"
#' @param newdata An optional data frame in which to look for variables with
#' which to predict. If omitted, the fitted values are used.
#' @param type "dur" or "prob". Default "dur".
#' @param distrx For type="prob", a vector of dispensing gaps.
#' @param quantile For type="dur", quantile of distribution. Default 0.8
#' @param se.fit A switch indicating if standard errors are required
#' @param na.action function determining what should be done with missing values in
#' `newdata`. The default is to predict `NA`
#' @param ... further arguments passed to or from other methods
#'
#' @return A vector of predictions
#' @export
#' @importFrom stats pnorm pweibull pexp
setMethod("predict", "wtd",
          function(object, newdata=NULL, type="dur", iadmean=F, distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {

            # Lognormal distribution
            if(object@dist=="lnorm") {

              if(type=="dur") {

                mu <- object@fullcoef[2]
                lnsigma <- object@fullcoef[3]

                  if(!iadmean) {

                    out <- exp(qnorm(quantile)*exp(lnsigma)+mu)

                  } else {

                    out <- exp(mu + .5 * exp(lnsigma)^2)

                  }

              } else if(type=="prob") {

                mu <- object@fullcoef[2]
                lnsigma <- object@fullcoef[3]

                out <- pnorm(-(log(distrx)-mu)/exp(lnsigma))
              }

              # Weibull distribution
            } else if(object@dist=="weib") {

              if(type=="dur") {

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]

                  if(!iadmean) {

                    out <- (-log(1-quantile))^(1/exp(lnalpha))*exp(lnbeta)

                  } else  {

                    out <-  exp(-lnbeta)^exp(-lnalpha) * exp(lgamma(1 + exp(- lnalpha)))

                  }

              } else if(type=="prob") {

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]
                ### lnalpha is the shape on the log-scale (lnalpha = 0 is exponential)
                # out <- pweibull(-((distrx*exp(lnbeta))^exp(lnalpha)), shape = lnalpha)
                out <- exp(-((distrx*exp(lnbeta))^exp(lnalpha)))
              }

              # Exponential distribution
            } else if(object@dist=="exp") {

              if(type=="dur") {

                lnbeta <- object@fullcoef[2]

                  if(!iadmean) {

                    out <- (-log(1-quantile))/exp(lnbeta)

                  } else {

                    out <- exp(-lnbeta)

                  }

              } else if(type=="prob") {

                lnbeta <- object@fullcoef[2]

                out <- exp(-exp(lnbeta)*distrx)
              }

            }

            out
          })
