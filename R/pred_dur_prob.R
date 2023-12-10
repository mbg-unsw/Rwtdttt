#' Title
#'
#' @param wtd
#'
#' @return
#' @export
#'
#' @examples
setMethod("predict", "wtd",
          function(object, newdata=NULL, type="dur", family = "lnorm", distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {

            # Lognormal distribution
            if(family=="lnorm") {

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
            } else if(family=="weibull") {

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
            } else if(family=="exp") {

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
