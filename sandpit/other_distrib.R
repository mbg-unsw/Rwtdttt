# How to save in the wtd object (here x.w) the family as an object?
# In order to use it in the if else statement within the predict function

# I can save it in a list as an output of dwtdttt function.
# But then I cannot give an object with multiple arguments to the wtdttt function
# And I cannot save it from the output of wtdttt

#################################################################################

# density functions

dwtdttt <- function(x, logitp, par1, par2, log = FALSE, family = "lnorm") {

  if (family=="lnorm") {
    prob <- exp(logitp) / (1 + exp(logitp))
    sigma <- exp(par2)
    mean <- exp(par1 + exp(2 * par2)/2)

    if (log == FALSE) {
      d <- prob * pnorm(log(x), par1, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta
    } else {
      d <- log(prob * pnorm(log(x), par1, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta)
    }

  } else if (family=="weibull") {
    prob <- exp(logitp) / (1 + exp(logitp))
    alpha <- exp(par1)
    beta <- exp(par2)

    if (log == FALSE) {
      d <- prob * exp(-((x*exp(par2))^exp(par1)) - lgamma(1+1/exp(par1))) * exp(par2) + (1-prob)/delta
    } else {
      d <- log(prob * exp(-((x*exp(par2))^exp(par1)) - lgamma(1+1/exp(par1))) * exp(par2) + (1-prob)/delta)
    }
  } else if (family=="exponential") {
    prob <- exp(logitp) / (1 + exp(logitp))
    alpha <- par1^0
    beta <- exp(par2)

    if (log == FALSE) {
      d <- prob * exp(-((x*exp(par2))^alpha) - lgamma(1+1/alpha)) * exp(par2) + (1-prob)/delta
    } else {
      d <- log(prob * exp(-((x*exp(par2))^alpha) - lgamma(1+1/alpha)) * exp(par2) + (1-prob)/delta)
    }
  }

  # return(density=d, family=family)
  return(density=d)
}


# wtd

wtdttt <- function(form, parameters=NULL, data, start, end, reverse=F,
                   subset, na.action=na.pass, init, control=NULL, ...) {

  out <- mle2(form, parameters = parameters,
              start = init, data = data)

  as(out, "wtd")

  return(list(out=out, fam=family))
}


# predict

setMethod("predict", "wtd",
          function(object, newdata=NULL, type="dur", family = "lnorm", distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {

            # Lognormal distribution
            if(family=="lnorm") {

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

                out <- pweibull(-((distrx*exp(lnbeta))^exp(lnalpha)))
              }

              # Exponential distribution
            } else if(family=="exp") {

              if(type=="dur") {
                # Case with iadmean==FALSE

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]

                out <- (-log(1-quantile))*exp(lnbeta)

              } else if(type=="prob") {

                lnalpha <- object@fullcoef[2]
                lnbeta <- object@fullcoef[3]

                out <- pexp(-(distrx*exp(lnbeta)))
              }

            }


            out
          })


################## TEST

x.w <- wtdttt(obstime ~ dwtdttt(logitp, par1, par2, family = "exponential"), data=x,
              start=as.Date('2014-01-01'), end=as.Date('2014-12-31'),
              init = list(logitp = 0, par1 = log(delta/5), par2 = 0))


