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
#' @importFrom stats pnorm pweibull pexp setNames
#' @importFrom Deriv Deriv
setMethod("predict", "wtd",
          function(object, prediction.data=NULL, type="dur", iadmean=F, distrx=NULL, quantile=0.8,
                   se.fit=FALSE, na.action=na.pass, ...) {

      # 03/02/25
      if (!is.null(prediction.data)) {

        prediction.data <- as.data.table(prediction.data)
        prediction.data <- na.action(prediction.data)

      }

      ##

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

            parnames_din <- sapply(strsplit(parm_form, "~"), `[`, 1)

            vpos <- list()
            for (i in seq(along=parm_form)) {
              vname <- vars[i]      ## name of variable
              vpos[[vname]] <- which(parnames_din==vname)
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
                vname <- parnames[i]      ## name of variable
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

                    # 23/01/25: implementing delta formula to estimate uncertainty

                    parnames_mu <- grep("mu", names(object@coef), value=T, fixed=F, invert=F)

                    parnames_mu <- gsub("\\(|\\)", "", parnames_mu)

                    # constructing part of the expression related to mu

                    expr_mu <- NULL

                    for (i in seq_along(parnames_mu)) {

                      addendo <- bquote(mm2[,.(i)] * .(as.name(parnames_mu[i])))

                      if (is.null(expr_mu)) {

                        expr_mu <- addendo

                      } else {

                        expr_mu <- bquote(.(expr_mu) + .(addendo))

                      }

                    }

                    parnames_lnsigma <- grep("lnsigma", names(object@coef), value=T, fixed=F, invert=F)

                    parnames_lnsigma <- gsub("\\(|\\)", "", parnames_lnsigma)

                    # constructing part of the expression related to lnsigma

                    expr_addend_lnsigma <- NULL

                    for (i in seq_along(parnames_lnsigma)) {

                      addendo <- bquote(mm3[,.(i)] * .(as.name(parnames_lnsigma[i])))

                      if (is.null(expr_addend_lnsigma)) {

                        expr_addend_lnsigma <- addendo

                      } else {

                        expr_addend_lnsigma <- bquote(.(expr_addend_lnsigma) + .(addendo))

                      }

                    }

                    expr_lnsigma <- bquote(quant*exp(.(expr_addend_lnsigma)))

                    dur <- bquote(exp(.(expr_mu) + .(expr_lnsigma)) )


                    # function to compute partial derivatives

                    compute_deriv <- function(param) {

                      deriv <- Deriv(dur, param)
                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- qnorm(quantile)

                      unique(eval(deriv, values))

                    }

                    # apply the function to compute partial derivatives to the formula mu-part

                    deriv_mu <- lapply(parnames_mu, compute_deriv)
                    names(deriv_mu) <- parnames_mu

                    max_len <- max(sapply(deriv_mu, length))

                    force_length <- function(vec, target_length) {
                      c(vec, rep(0, target_length - length(vec)))
                    }

                    deriv_mu <- lapply(deriv_mu, force_length, target_length = max_len)


                    # apply the function to compute partial derivatives to the formula lnsigma-part

                    deriv_lnsigma <- lapply(parnames_lnsigma, compute_deriv)
                    names(deriv_lnsigma) <- parnames_lnsigma

                    max_len <- max(sapply(deriv_lnsigma, length))

                    force_length <- function(vec, target_length) {
                      c(vec, rep(0, target_length - length(vec)))
                    }

                    deriv_lnsigma <- lapply(deriv_lnsigma, force_length, target_length = max_len)

                    deriv_mu_m <- matrix(unlist(deriv_mu), ncol = max_len, byrow = T)
                    deriv_lnsigma_m <- matrix(unlist(deriv_lnsigma), ncol = max_len, byrow = T)

                    dpart_m <- rbind(deriv_mu_m, deriv_lnsigma_m)


                    out <- vector()

                    for  (i in 1:ncol(dpart_m)) {

                      dpart <- matrix(dpart_m[,i], nrow = 1)

                      varcov <- object@vcov

                      selection <- grep("^(mu|lnsigma)", rownames(varcov))

                      cov_dur <- dpart %*% varcov[selection, selection] %*% t(dpart)

                      se_dur <- as.vector(sqrt(cov_dur))


                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- qnorm(quantile)

                      dur_num <- unique(eval(dur, values))
                      dur_num_v <- eval(dur, values)

                      z <- round(dur_num/se_dur,7)

                      p_value <- 2*pnorm(z, lower.tail = F)

                      dur_ci_lower <- round(dur_num-qnorm(0.975)*se_dur,7)
                      dur_ci_upper <- round(dur_num+qnorm(0.975)*se_dur,7)


                      # tmp <- data.frame(variable = as.character(unique(mm_names_2)[i,]), duration = round(dur_num,7)[i], CI95 = dur_ci[i], SE = round(se_dur,7), z = round(dur_num/se_dur,7)[i])

                      # S4 object as output
                      # tmp <- data.frame(variable = unique(mm_names)[i,], Estimate = round(dur_num,7)[i], SE = round(se_dur,7), z = z[i], p_value = p_value[i], Lower.95= dur_ci_lower[i], Upper.95= dur_ci_upper[i], row.names = NULL)

                      # list as output
                      # tmp <- list(Estimate = round(dur_num_v,7), SE = round(se_dur,7))#, z = z[i], p_value = p_value[i], Lower.95= dur_ci_lower[i], Upper.95= dur_ci_upper[i])

                      # vector as output
                      tmp <- round(dur_num_v,7)

                      # tmp <- setNames(tmp, replace(names(tmp), names(tmp) %in% c("SE", "z", "p_value"), c("Std. Error", "z value", "Pr(z)")) )

                      out <- tmp


                      # if(!se.fit) {
                      #
                      #   out <- dur_num_v
                      #
                      # } else {
                      #
                      #   # out <- rbind(out, tmp)
                      #   out <- tmp
                      #
                      # }


                     }


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
                vname <- parnames[i]      ## name of variable
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

                    # 11/03/25: implementing delta formula to estimate uncertainty

                    parnames_lnalpha <- grep("lnalpha", names(object@coef), value=T, fixed=F, invert=F)

                    parnames_lnalpha <- gsub("\\(|\\)", "", parnames_lnalpha)

                    # constructing part of the expression related to lnalpha

                    expr_lnalpha <- NULL

                    for (i in seq_along(parnames_lnalpha)) {

                      addendo <- bquote(mm2[,.(i)] * .(as.name(parnames_lnalpha[i])))

                      if (is.null(expr_lnalpha)) {

                        expr_lnalpha <- addendo

                      } else {

                        expr_lnalpha <- bquote(.(expr_lnalpha) + .(addendo))

                      }

                    }

                    parnames_lnbeta <- grep("lnbeta", names(object@coef), value=T, fixed=F, invert=F)

                    parnames_lnbeta <- gsub("\\(|\\)", "", parnames_lnbeta)

                    # constructing part of the expression related to lnbeta

                    expr_lnbeta <- NULL

                    for (i in seq_along(parnames_lnbeta)) {

                      addendo <- bquote(mm3[,.(i)] * .(as.name(parnames_lnbeta[i])))

                      if (is.null(expr_lnbeta)) {

                        expr_lnbeta <- addendo

                      } else {

                        expr_lnbeta <- bquote(.(expr_lnbeta) + .(addendo))

                      }

                    }

                    dur <- bquote( (-log(1-quant))^(1/exp(.(expr_lnalpha)))/exp(.(expr_lnbeta)))


                    # function to compute partial derivatives

                    compute_deriv <- function(param) {

                      deriv <- Deriv(dur, param)
                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- quantile

                      unique(eval(deriv, values))

                    }

                    # apply the function to compute partial derivatives to the formula lnalpha-part

                    deriv_lnalpha <- lapply(parnames_lnalpha, compute_deriv)
                    names(deriv_lnalpha) <- parnames_lnalpha

                    max_len <- max(sapply(deriv_lnalpha, length))

                    force_length <- function(vec, target_length) {
                      c(vec, rep(0, target_length - length(vec)))
                    }

                    deriv_lnalpha <- lapply(deriv_lnalpha, force_length, target_length = max_len)


                    # apply the function to compute partial derivatives to the formula lnbeta-part

                    deriv_lnbeta <- lapply(parnames_lnbeta, compute_deriv)
                    names(deriv_lnbeta) <- parnames_lnbeta

                    max_len <- max(sapply(deriv_lnbeta, length))

                    force_length <- function(vec, target_length) {
                      c(vec, rep(0, target_length - length(vec)))
                    }

                    deriv_lnbeta <- lapply(deriv_lnbeta, force_length, target_length = max_len)

                    deriv_lnalpha_m <- matrix(unlist(deriv_lnalpha), ncol = max_len, byrow = T)
                    deriv_lnbeta_m <- matrix(unlist(deriv_lnbeta), ncol = max_len, byrow = T)

                    dpart_m <- rbind(deriv_lnalpha_m, deriv_lnbeta_m)



                    out <- vector()

                    for  (i in 1:ncol(dpart_m)) {

                      dpart <- matrix(dpart_m[,i], nrow = 1)

                      varcov <- object@vcov

                      selection <- grep("^(lnalpha|lnbeta)", rownames(varcov))

                      cov_dur <- dpart %*% varcov[selection, selection] %*% t(dpart)

                      se_dur <- as.vector(sqrt(cov_dur))


                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- quantile


                      dur_num <- unique(eval(dur, values))
                      dur_num_v <- eval(dur, values)

                      z <- round(dur_num/se_dur,7)

                      p_value <- 2*pnorm(z, lower.tail = F)

                      dur_ci_lower <- round(dur_num-qnorm(0.975)*se_dur,7)
                      dur_ci_upper <- round(dur_num+qnorm(0.975)*se_dur,7)

                      # tmp <- data.frame(variable = as.character(unique(mm_names_2)[i,]), duration = round(dur_num,7)[i], CI95 = dur_ci[i], SE = round(se_dur,7), z = round(dur_num/se_dur,7)[i])

                      # tmp <- data.frame(variable = unique(mm_names)[i,], Estimate = round(dur_num,7)[i], SE = round(se_dur,7), z = z[i], p_value = p_value[i], Lower.95= dur_ci_lower[i], Upper.95= dur_ci_upper[i], row.names = NULL)
                      # tmp <- setNames(tmp, replace(names(tmp), names(tmp) %in% c("SE", "z", "p_value"), c("Std. Error", "z value", "Pr(z)")) )
                      #
                      # out <- rbind(out, tmp)

                      # vector as output
                      tmp <- round(dur_num_v,7)

                      out <- tmp

                    }

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
                vname <- parnames[i]      ## name of variable
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

                    # out <- (-log(1-quantile))/exp(lnbeta)
                    # out <- cbind(out, mm_names)

                    # 11/03/25: implementing delta formula to estimate uncertainty

                    parnames_lnbeta <- grep("lnbeta", names(object@coef), value=T, fixed=F, invert=F)

                    parnames_lnbeta <- gsub("\\(|\\)", "", parnames_lnbeta)

                    # constructing part of the expression related to lnbeta

                    expr_lnbeta <- NULL

                    for (i in seq_along(parnames_lnbeta)) {

                      addendo <- bquote(mm2[,.(i)] * .(as.name(parnames_lnbeta[i])))

                      if (is.null(expr_lnbeta)) {

                        expr_lnbeta <- addendo

                      } else {

                        expr_lnbeta <- bquote(.(expr_lnbeta) + .(addendo))

                      }

                    }

                    dur <- bquote( (-log(1-quant))/exp(.(expr_lnbeta)))


                    # function to compute partial derivatives

                    compute_deriv <- function(param) {

                      deriv <- Deriv(dur, param)
                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- quantile

                      unique(eval(deriv, values))

                    }


                    # apply the function to compute partial derivatives to the formula lnbeta-part

                    deriv_lnbeta <- lapply(parnames_lnbeta, compute_deriv)
                    names(deriv_lnbeta) <- parnames_lnbeta

                    max_len <- max(sapply(deriv_lnbeta, length))

                    force_length <- function(vec, target_length) {
                      c(vec, rep(0, target_length - length(vec)))
                    }

                    deriv_lnbeta <- lapply(deriv_lnbeta, force_length, target_length = max_len)

                    deriv_lnbeta_m <- matrix(unlist(deriv_lnbeta), ncol = max_len, byrow = T)

                    dpart_m <- deriv_lnbeta_m



                    out <- vector()

                    for  (i in 1:ncol(dpart_m)) {

                      dpart <- matrix(dpart_m[,i], nrow = 1)

                      varcov <- object@vcov

                      selection <- grep("^(lnbeta)", rownames(varcov))

                      cov_dur <- dpart %*% varcov[selection, selection] %*% t(dpart)

                      se_dur <- as.vector(sqrt(cov_dur))

                      values <- as.list(setNames(object@coef, gsub("\\(|\\)", "", names(object@coef)) ))
                      values$quant <- quantile

                      dur_num <- unique(eval(dur, values))
                      dur_num_v <- eval(dur, values)

                      z <- round(dur_num/se_dur,7)

                      p_value <- 2*pnorm(z, lower.tail = F)

                      dur_ci_lower <- round(dur_num-qnorm(0.975)*se_dur,7)
                      dur_ci_upper <- round(dur_num+qnorm(0.975)*se_dur,7)

                      # tmp <- data.frame(variable = as.character(unique(mm_names_2)[i,]), duration = round(dur_num,7)[i], CI95 = dur_ci[i], SE = round(se_dur,7), z = round(dur_num/se_dur,7)[i])

                      # tmp <- data.frame(variable = unique(mm_names)[i,], Estimate = round(dur_num,7)[i], SE = round(se_dur,7), z = z[i], p_value = p_value[i], Lower.95= dur_ci_lower[i], Upper.95= dur_ci_upper[i], row.names = NULL)
                      # tmp <- setNames(tmp, replace(names(tmp), names(tmp) %in% c("SE", "z", "p_value"), c("Std. Error", "z value", "Pr(z)")) )
                      #
                      # out <- rbind(out, tmp)

                      # vector as output
                      tmp <- round(dur_num_v,7)

                      out <- tmp

                    }

                  } else {

                    out <- exp(-lnbeta)

                  }

              } else if(type=="prob") {

                out <- exp(-exp(lnbeta)*distrx)
              }

            }

            if(type=="dur") {

              # return(unique(out))
              return(out)

            } else if(type=="prob") {

              return(out)

            }




          })
