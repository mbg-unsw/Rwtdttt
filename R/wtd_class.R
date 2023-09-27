# Rwtdttt
# Combine Henrik, Malcolm & Sabrina's progress so far
# Create a "wtd" model class and use it to set up a 'predict' method
# This is a proof of concept to show how we can overload 'predict()' etc

library(bbmle)
library(class)

# Register a 'wtd' class, inheriting from 'mle2'

setClass("wtd", contains="mle2")

delta <- 1

dlnorm <- function(x, logitp, mu, lnsigma, log = FALSE) {

    prob <- exp(logitp) / (1 + exp(logitp))
    sigma <- exp(lnsigma)
    mean <- exp(mu + exp(2 * lnsigma)/2)

    if (log == FALSE) {
      prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta
    } else {
      log(prob * pnorm(log(x), mu, sigma, lower.tail = FALSE) / mean + (1 - prob) / delta)
    }

}

wtdttt <- function(form, parameters=NULL, data, start, end, reverse=F,
	subset, na.action=na.pass, init, control=NULL, ...) {

	out <- mle2(form, parameters = parameters,
             start = init, data = data)

	as(out, "wtd") # XXXX may need to store more things in the output obj
}

# XXXX only handling the lnorm case for now
# Copied Sabrina's code, is there a normalising factor/logitp missing?

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
	
