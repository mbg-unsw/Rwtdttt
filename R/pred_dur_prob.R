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
#' #' @section Data format:
#' Covariates in the data provided for wtdttt, used for estimation, must be in the prediction data as well -if used- with identical names
#' The class of the covariate must be the same in the dataset used for estimation and in the one used for prediction - if different - i.e. packsize used as a factor.
#'
#' @param object a fitted object of class inheriting from "wtd"
#' @param prediction.data An optional data frame in which to look for variables with
#' which to predict. If omitted, the fitted values are used.
#' @param type "dur" or "prob". Default "dur".
#' @param iadmean logical; if T, mean duration is predicted.
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
          function(object, prediction.data=NULL, type="dur", iadmean=F, distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {


       if(!is.null(distrx)) {



            if(is(distrx, "Date"))
              conttime <- 0
            else if(is(distrx, "numeric"))
              conttime <- 1


            if(object@isreverse) {

              if(!conttime)
                distrx <- 0.5 + as.double(as.Date(object@end) - distrx, units="days")
              else
                distrx <- as.numeric(object@end) - distrx


            } else {

              if(!conttime)
                distrx <- 0.5 + as.double(distrx - as.Date(object@start), units="days")
              else
                distrx <- distrx - as.numeric(object@start)

            }

       }

            ##################

            parm_form <- unlist(strsplit(gsub(" ", "", unlist(strsplit(object@formula, ":", fixed=T))[2]), ",", fixed=T))

            # !! parnames e vars are the same (keep only one)

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


            # Lognormal distribution
            if(object@dist=="lnorm") {

              if (is.null(prediction.data)) {

              mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
              mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(object@data))
              mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(object@data))

              mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
              mm_names_2 <- model.frame(formula(models[vpos[["mu"]]]), data=as.data.frame(object@data))
              mm_names_3 <- model.frame(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(object@data))

              check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2], dim(mm_names_3)[2]))
              vec <- list(mm_names_1, mm_names_2, mm_names_3)
              mm_names <- data.frame(vec[check])

              } else {

                for (i in seq_along(parnames)) {

                  if (length(labels(terms(as.formula(parm_form[i]))))!=0) {

                    if ((sum(!labels(terms(as.formula(parm_form[i]))) %in% names(prediction.data)==T)) >=1) {

                      stop("Covariates used in the estimation are not in the prediction dataset (new data)")

                    }

                  }

                }

                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(prediction.data))
                mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(prediction.data))

                mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm_names_2 <- model.frame(formula(models[vpos[["mu"]]]), data=as.data.frame(prediction.data))
                mm_names_3 <- model.frame(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(prediction.data))

                check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2], dim(mm_names_3)[2]))
                vec <- list(mm_names_1, mm_names_2, mm_names_3)
                mm_names <- data.frame(vec[check])

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

              if (is.null(prediction.data)) {

                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
                mm2 <- model.matrix(formula(models[vpos[["lnalpha"]]]), data=as.data.frame(object@data))
                mm3 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(object@data))

                mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
                mm_names_2 <- model.frame(formula(models[vpos[["lnalpha"]]]), data=as.data.frame(object@data))
                mm_names_3 <- model.frame(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(object@data))

                check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2], dim(mm_names_3)[2]))
                vec <- list(mm_names_1, mm_names_2, mm_names_3)
                mm_names <- data.frame(vec[check])

              } else {

                for (i in seq_along(parnames)) {

                  if (length(labels(terms(as.formula(parm_form[i]))))!=0) {

                    if ((sum(!labels(terms(as.formula(parm_form[i]))) %in% names(prediction.data)==T)) >=1) {

                      stop("Covariates used in the estimation are not in the prediction dataset (new data)")

                    }

                  }

                }

                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm2 <- model.matrix(formula(models[vpos[["lnalpha"]]]), data=as.data.frame(prediction.data))
                mm3 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(prediction.data))

                mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm_names_2 <- model.frame(formula(models[vpos[["lnalpha"]]]), data=as.data.frame(prediction.data))
                mm_names_3 <- model.frame(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(prediction.data))

                check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2], dim(mm_names_3)[2]))
                vec <- list(mm_names_1, mm_names_2, mm_names_3)
                mm_names <- data.frame(vec[check])

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

                    out <- (-log(1-quantile))^(1/exp(lnalpha))/exp(lnbeta)
                    out <- cbind(out, mm_names)

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

              if (is.null(prediction.data)) {

                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
                mm2 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(object@data))

                mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(object@data))
                mm_names_2 <- model.frame(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(object@data))

                check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2]))
                vec <- list(mm_names_1, mm_names_2)
                mm_names <- data.frame(vec[check])

              } else {

                for (i in seq_along(parnames)) {

                  if (length(labels(terms(as.formula(parm_form[i]))))!=0) {

                    if ((sum(!labels(terms(as.formula(parm_form[i]))) %in% names(prediction.data)==T)) >=1) {

                      stop("Covariates used in the estimation are not in the prediction dataset (new data)")

                    }

                  }

                }

                mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm2 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(prediction.data))

                mm_names_1 <- model.frame(formula(models[vpos[["logitp"]]]), data=as.data.frame(prediction.data))
                mm_names_2 <- model.frame(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(prediction.data))

                check <- which.max(c(dim(mm_names_1)[2], dim(mm_names_2)[2]))
                vec <- list(mm_names_1, mm_names_2)
                mm_names <- data.frame(vec[check])

              }

              # design matrix multiplied by estimates
              est <- list()
              for (i in seq_along(parnames)) {
                vname <- vars[i]      ## name of variable
                est[[vname]] <- object@coef[grepl(vname, names(object@coef))]
              }

              # compute parameters (i.e. mu) used to predict duration and probabilities
              mm <- c("mm1", "mm2")

              for (i in seq_along(parnames)) {

                x <- get(mm[i]) %*% matrix(unlist(est[i]))
                assign(parnames[i], x)

              }

              if(type=="dur") {

                  if(!iadmean) {

                    out <- (-log(1-quantile))/exp(lnbeta)
                    out <- cbind(out, mm_names)

                  } else {

                    out <- exp(-lnbeta)

                  }

              } else if(type=="prob") {

                out <- exp(-exp(lnbeta)*distrx)
              }

            }

            if(type=="dur") {

              return(unique(out))

            } else if(type=="prob") {

              return(out)

            }




          })
