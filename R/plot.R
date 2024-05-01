# plot - R functions and documentation

# make sure class definition is listed first in DESCRIPTION Collate:
#' @include wtd-class.R
NULL

#' Make WTD diagnostic plots
#'
#' Make diagnostic plots showing the fit of an estimated parametric Waiting Time
#' Distribution (WTD) with respect to the observed histogram of prescription
#' redemptions.
#'
#' @param wtd wtd object, typically result of wtdttt
#' @export
#' @importFrom graphics hist curve
setMethod("plot", "wtd",
          function(object, x, y, ...) {

            h <- hist(object@data[[object@depvar]], freq=F,
                      main=object@dist, xlab=object@depvar)
            d <- h$breaks[2] - h$breaks[1]

            if(object@dist=="lnorm") {
              logitp <- object@fullcoef[1]
              mu <- object@fullcoef[2]
              lnsigma <- object@fullcoef[3]

              curve(dlnorm(x, logitp, mu, lnsigma, object@delta),
                    from=0.1, to=max(object@data[[object@depvar]]), add=T)
            } else if(object@dist=="weib") {

              logitp <- object@fullcoef[1]
              lnalpha <- object@fullcoef[2]
              lnbeta <- object@fullcoef[3]

              curve(dweib(x, logitp, lnalpha, lnbeta, object@delta),
                    from=0.1, to=max(object@data[[object@depvar]]), add=T)

            } else if(object@dist=="exp") {

              logitp <- object@fullcoef[1]
              lnbeta <- object@fullcoef[2]

              curve(dexp(x, logitp, lnbeta, object@delta),
                    from=0.1, to=max(object@data[[object@depvar]]), add=T)

            }
          })

