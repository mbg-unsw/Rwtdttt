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
#' @param x ignored
#' @param y ignored
#' @param ... other graphical parameters (see `par`)
#' @export
#' @importFrom graphics hist curve
setMethod("plot", "wtd",
          function(wtd, x = NULL, y = NULL, ...) {

            h <- hist(wtd@data[[wtd@depvar]], freq=F,
                      main=wtd@dist, xlab=wtd@depvar)
            d <- h$breaks[2] - h$breaks[1]

            if(wtd@dist=="lnorm") {
              logitp <- wtd@fullcoef[1]
              mu <- wtd@fullcoef[2]
              lnsigma <- wtd@fullcoef[3]

              curve(dlnorm(x, logitp, mu, lnsigma, wtd@delta),
                    from=0.1, to=max(wtd@data[[wtd@depvar]]), add=T)
            } else if(wtd@dist=="weib") {

              logitp <- wtd@fullcoef[1]
              lnalpha <- wtd@fullcoef[2]
              lnbeta <- wtd@fullcoef[3]

              curve(dweib(x, logitp, lnalpha, lnbeta, wtd@delta),
                    from=0.1, to=max(wtd@data[[wtd@depvar]]), add=T)

            } else if(wtd@dist=="exp") {

              logitp <- wtd@fullcoef[1]
              lnbeta <- wtd@fullcoef[2]

              curve(dexp(x, logitp, lnbeta, wtd@delta),
                    from=0.1, to=max(wtd@data[[wtd@depvar]]), add=T)

            }
          })

