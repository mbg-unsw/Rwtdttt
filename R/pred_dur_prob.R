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
#' @param wtd a fitted object of class inheriting from "wtd"
#'
#' @return A vector of predictions
#' @export
#' @importFrom stats pnorm pweibull pexp
setMethod("predict", "wtd",
          function(object, newdata=NULL, type="dur", distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {

            # Lognormal distribution
            if(object@dist=="lnorm") {

              if(type=="dur") {
                # Case with iadmean==FALSE

                mu <- object@fullcoef[2]
                lnsigma <- object@fullcoef[3]

                # out <- exp(qnorm(quantile)*exp(lnsigma)+mu)
                out <- exp(mu + .5 * exp(lnsigma)^2)

              } else if(type=="prob") {

                mu <- object@fullcoef[2]
                lnsigma <- object@fullcoef[3]

                out <- pnorm(-(log(distrx)-mu)/exp(lnsigma))
              }

              # Weibull distribution
            } else if(object@dist=="weib") {

              if(type=="dur") {
                # Case with iadmean==FALSE

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]

                out <- (-log(1-quantile))^(1/exp(lnalpha))*exp(lnbeta)

              } else if(type=="prob") {

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]
                ### which is the shape?
                out <- pweibull(-((distrx*exp(lnbeta))^exp(lnalpha)), shape = lnalpha)
              }

              # Exponential distribution
            } else if(object@dist=="exp") {

              if(type=="dur") {
                # Case with iadmean==FALSE

                lnbeta <- object@fullcoef[2]

                out <- (-log(1-quantile))*exp(lnbeta)

              } else if(type=="prob") {

                lnbeta <- object@fullcoef[2]

                out <- pexp(-(distrx*exp(lnbeta)))
              }

            }

            out
          })
