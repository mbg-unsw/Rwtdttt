# calculate sandwich vcov

#' Title
#'
#' @param fit
#'
#' @return
#' @importFrom stats model.matrix
#' @importFrom numDeriv grad
#'
#' @examples
sand_vcov <- function(fit) {
  # parse model formula components
  parm_form <- unlist(strsplit(gsub(unlist(strsplit(fit@formula, ":", fixed=T))[2], " ", ""), ",", fixed=T))

#####
# based on code from bbmle::calc_mle2_function
# FIXME: redo without special case code for each density type
# FIXME: redo to handle non-standard parameter naming

  parnames <- names(fit@call$start)

  if (!is.na(parm_form) && length(parm_form)>0) {
    ## linear model specified for some parameters
    vars <- sapply(strsplit(parm_form, "~", fixed=T),"[",1)
    models <-  sapply(strsplit(parm_form, "~", fixed=T),"[",2)
  } else {parm_form <- c(); vars <- c(); models <- c()}

  # FIXME for any missing components, substitute "~1"

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

    score_mat <- t(sapply(1:length(fit@data[[1]]),

                        function(i) {

                          logl <- function(x) {

                            if(fit@dist=="lnorm") {

                              # FIXME this is very ugly
                              # XXXX can we use fit@minuslogl somehow instead?


                              mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(fit@data))[i,,drop=F]
                              mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(fit@data), drop=F)[i,,drop=F]
                              mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(fit@data), drop=F)[i,,drop=F]

                              dlnorm(as.numeric(getElement(fit@data, fit@depvar)[i]),

                                      logitp=t(x[1:ncol(mm1)]) %*% mm1,

                                      mu=t(x[(ncol(mm1)+1):(ncol(mm1)+ncol(mm2))]) %*% mm2,

                                      lnsigma=t(x[(ncol(mm1)+ncol(mm2)+1):(ncol(mm1)+ncol(mm2)+ncol(mm3))]) %*% mm3,

                                      delta=fit@delta,

                                      log=T)

                          } else if(fit@dist=="weib") {

                            mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(fit@data))[i,,drop=F]
                            mm2 <- model.matrix(formula(models[vpos[["lnalpha"]]]), data=as.data.frame(fit@data))[i,,drop=F]
                            mm3 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(fit@data))[i,,drop=F]

                            dweib(as.numeric(getElement(fit@data, fit@depvar)[i]),

                                   logitp=t(x[1:ncol(mm1)]) %*% mm1,

                                   lnalpha=t(x[(ncol(mm1)+1):(ncol(mm1)+ncol(mm2))]) %*% mm2,

                                   lnbeta=t(x[(ncol(mm1)+ncol(mm2)+1):(ncol(mm1)+ncol(mm2)+ncol(mm3))]) %*% mm3,

                                   delta=fit@delta,

                                   log=T)

                         } else if(fit@dist=="exp") {

                          mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(fit@data))[i,,drop=F]
                          mm2 <- model.matrix(formula(models[vpos[["lnbeta"]]]), data=as.data.frame(fit@data))[i,,drop=F]

                          dexp(as.numeric(getElement(fit@data, fit@depvar)[i]),

                                 logitp=t(x[1:ncol(mm1)]) %*% mm1,

                                 lnbeta=t(x[(ncol(mm1)+1):(ncol(mm1)+ncol(mm2))]) %*% mm2,

                                 delta=fit@delta,

                                 log=T)

                        }
                      }



                          grad(logl, fit@coef)

                        }

  ))

  colnames(score_mat) <- names(fit@coef)

  bread <- fit@vcov

  applyTapplySum <- function(X,index) apply(X, 2, function(col) tapply(col, index, sum))

  psi <- applyTapplySum(as.matrix(score_mat), getElement(fit@data, fit@idvar)) # sum by cluster

  meat <- crossprod(as.matrix(psi))

  rownames(meat) <- colnames(meat) <- colnames(psi)

  nid <- NROW(unique(getElement(fit@data, fit@idvar)))

  (nid/(nid-1)) * (bread %*% meat %*% bread)
}