# plot - R functions and documentation

# Make sure wtd class definition is loaded first
#' @include wtdttt.R

#' Make WTD diagnostic plots
#'
#' Make diagnostic plots showing the fit of an estimated parametric Waiting Time
#' Distribution (WTD) with respect to the observed histogram of prescription
#' redemptions.
#' HS 240108: I think this needs to incorporate information on time period
#' of observations (start and end specified in call to wtdttt)
#'
#' @param wtd wtd object, typically result of wtdttt
#' @export
#' @importFrom graphics curve hist
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

