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
  parm_form <- unlist(strsplit(unlist(strsplit(fit@formula, ":", fixed=T))[2], ",", fixed=T))
  # FIXME for any missing components, substitute "~1"
    score_mat <- t(sapply(1:nrow(fit@data),

                        function(i) {

                          logl <- function(x) {

                            if(fit@dist=="lnorm") {

                              # FIXME this is very ugly
                              # FIXME check order of columns in fit@coef
                              # FIXME check order of parm_form components
                              # XXXX can we use fit@minuslogl somehow instead?

                              mm1 <- model.matrix(formula(parm_form[1]), data=fit@data)[i,]
                              mm2 <- model.matrix(formula(parm_form[2]), data=fit@data)[i,]
                              mm3 <- model.matrix(formula(parm_form[3]), data=fit@data)[i,]

                              dlnorm(as.numeric(fit@data[i, fit@depvar]),

                                      logitp=t(x[1:ncol(mm1)]) %*% mm1,

                                      mu=t(x[(ncol(mm1)+1):(ncol(mm1)+ncol(mm2))]) %*% mm2,

                                      lnsigma=t(x[(ncol(mm1)+ncol(mm2)+1):(ncol(mm1)+ncol(mm2)+ncol(mm3))]) %*% mm3,

                                      delta=fit@delta,

                                      log=T)

                          } else if(fit@dist=="weib") {

                            mm1 <- model.matrix(formula(parm_form[1]), data=fit@data)[i,]
                            mm2 <- model.matrix(formula(parm_form[2]), data=fit@data)[i,]
                            mm3 <- model.matrix(formula(parm_form[3]), data=fit@data)[i,]

                            dweib(as.numeric(fit@data[i, fit@depvar]),

                                   logitp=t(x[1:ncol(mm1)]) %*% mm1,

                                   lnalpha=t(x[(ncol(mm1)+1):(ncol(mm1)+ncol(mm2))]) %*% mm2,

                                   lnbeta=t(x[(ncol(mm1)+ncol(mm2)+1):(ncol(mm1)+ncol(mm2)+ncol(mm3))]) %*% mm3,

                                   delta=fit@delta,

                                   log=T)

                         } else if(fit@dist=="exp") {

                          mm1 <- model.matrix(formula(parm_form[1]), data=fit@data)[i,]
                          mm2 <- model.matrix(formula(parm_form[2]), data=fit@data)[i,]

                          dexp(as.numeric(fit@data[i, fit@depvar]),

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

  psi <- applyTapplySum(as.matrix(score_mat), fit@data[, fit@idvar]) # sum by cluster

  meat <- crossprod(as.matrix(psi))

  rownames(meat) <- colnames(meat) <- colnames(psi)

  nid <- NROW(unique(fit@data[, fit@idvar]))

  (nid/(nid-1)) * (bread %*% meat %*% bread)
}
