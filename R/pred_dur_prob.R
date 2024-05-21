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

              parm_form <- unlist(strsplit(gsub(" ", "", unlist(strsplit(object@formula, ":", fixed=T))[2]), ",", fixed=T))

              # !! parnames e vars sono uguali (ne tengo solo uno)

              parnames <- grep("delta", names(object@call$start), value=T, fixed=T, invert=T)

              if (typeof(parm_form)=="character" && length(parm_form)>0) {
                ## linear model specified for some parameters
                vars <- sapply(strsplit(parm_form, "~", fixed=T),"[",1)
                models <-  paste0("~", sapply(strsplit(parm_form, "~", fixed=T),"[",2))
              } else {parm_form <- c(); vars <- c(); models <- c()}

              for (i in seq(along=parnames)) {
                if (!(parnames[i] %in% vars)) {
                  vars <- c(vars, parnames[i])
                  models <- c(models, "~1")
                  parm_form <- c(parm_form, paste0(parnames[i], "~1"))
                }
              }

              vpos <- list()
              for (i in seq(along=parm_form)) {
                vname <- vars[i]      ## name of variable
                vpos[[vname]] <- which(parnames==vname)
              }

            # if(fit@dist=="lnorm") {

              if (is.null(newdata)) {

              mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
              mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(object@data))
              mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(object@data))

              mm_names <- model.frame(formula(models[vpos[["mu"]]]), data=as.data.frame(object@data))

              } else {

                for (i in seq_along(parnames)) {

                  if (length(labels(terms(as.formula(parm_form[i]))))!=0) {

                    if ((sum(!labels(terms(as.formula(parm_form[i]))) %in% names(newdata)==T)) >=1) {

                      stop("Covariates used in the estimation are not in the prediction dataset (new data)")

                    }

                  }

                }


                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(newdata))
                mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(newdata))
                mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(newdata))

              }

              # design matrix multiplied by estimates
              est <- list()
              for (i in seq_along(parnames)) {
                vname <- vars[i]      ## name of variable
                est[[vname]] <- object@coef[grepl(vname, names(object@coef))]
              }

              # compute parameters (i.e. mu) used to predict duration and probabilities
              mm <- c("mm1", "mm2", "mm3")

              for (i in seq_along(parnames)) {

                x <- get(mm[i]) %*% matrix(unlist(est[i]))
                assign(parnames[i], x)

              }

              if(type=="dur") {

                  if(!iadmean) {

                    out <- exp(qnorm(quantile)*exp(lnsigma)+mu)
                    out <- cbind(out, mm_names)

                  } else {

                    out <- exp(mu + .5 * exp(lnsigma)^2)

                  }

              } else if(type=="prob") {

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

            if(type=="dur") {

              return(unique(out))

            } else if(type=="prob") {

              return(out)

            }




          })
