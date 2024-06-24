# How to compute derivatives vectorized using the numericDeriv() function


library(bbmle)
library(Rwtdttt)
library(haven)
library(data.table)
library(readxl)

# load data
# test data for sandwich from Henrik

tmp <- haven::read_dta(system.file("extdata", "score_ex.dta", package="Rwtdttt"))

tmp$packcat <- factor(tmp$packsize)

cl <- tmp ## [order(tmp$pid),]

fit <- wtdttt(data = cl,
               rxtime ~ dlnorm(logitp, mu, lnsigma),
               parameters = list(logitp ~ packcat, mu ~ packcat, lnsigma ~ packcat),
               #id = "pid", # do this to stop wtdttt() from filtering the data
               start = 0, end = 1
)
parm_form <- unlist(strsplit(gsub(" ", "", unlist(strsplit(fit@formula, ":", fixed=T))[2]), ",", fixed=T))

#####
# based on code from bbmle::calc_mle2_function
# FIXME: painfully slow
# FIXME: redo without special case code for each density type
# FIXME: redo to handle non-standard parameter naming

parnames <- grep("delta", names(fit@call$start), value=T, fixed=T, invert=T)

if (typeof(parm_form)=="character" && length(parm_form)>0) {
  ## linear model specified for some parameters
  vars <- sapply(strsplit(parm_form, "~", fixed=T),"[",1)
  models <-  paste0("~", sapply(strsplit(parm_form, "~", fixed=T),"[",2))
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



mm1 <- model.matrix(formula(models[vpos[["logitp"]]]), data=as.data.frame(fit@data))
mm2 <- model.matrix(formula(models[vpos[["mu"]]]), data=as.data.frame(fit@data), drop=F)
mm3 <- model.matrix(formula(models[vpos[["lnsigma"]]]), data=as.data.frame(fit@data), drop=F)

mm <- cbind(mm1, mm2, mm3)

logitp_est <- mm1 %*% fit@coef[1:2]
mu_est <- mm2 %*% fit@coef[3:4]
lnsigma_est <- mm3%*% fit@coef[5:6]



myenv <- new.env()
myenv$logitp <- logitp_est
myenv$mu <- mu_est
myenv$lnsigma <- lnsigma_est
myenv$x <- as.numeric(getElement(fit@data, fit@depvar))

nD <- numericDeriv(quote(dlnorm(x, logitp, mu, lnsigma, log=T)), c("logitp", "mu", "lnsigma"), myenv)
grad_mat <- attr(nD, "gradient")

# also possible to just compute one derivative at a time
nD2 <- numericDeriv(quote(dlnorm(x, logitp, mu, lnsigma, log=T)), c("logitp"), myenv)
grad_mat2 <- diag(attr(nD2, "gradient"))

# The u-vectors in the Stata output (ml score)
grad_vecs <- cbind(as.numeric(getElement(fit@data, fit@depvar)),
                   diag(grad_mat[1:nrow(grad_mat), 1:nrow(grad_mat)]),
                   diag(grad_mat[1:nrow(grad_mat),
                                 (nrow(grad_mat) + 1):(2*nrow(grad_mat))]),
                   diag(grad_mat[1:nrow(grad_mat),
                                 (2*nrow(grad_mat) + 1):(3*nrow(grad_mat))]))
grad_vecs[1:10,] # This is the same as Stata's computation!! HOORRAY!!

# Each column should now be replicated and multiplied with corresponding column
# of model matrix
