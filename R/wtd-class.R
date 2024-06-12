# wtd-class - R class definition

# Register a 'wtd' class, inheriting from 'mle2'

#' An S4 class representing the result of maximum likelihood estimation for a WTD
#'
#' This class encapsulates results of maximum likelihood estimation for a
#' waiting time distribution.
#'
#' @slot delta value of the delta parameter
#' @slot dist name of the WTD distribution family: "lnorm", "weib" or "exp"
#' @slot depvar name of the dependent variable
#' @slot idvar (optional) name of the id variable
#' @slot isreverse logical; indicates whether a forward or backward distribution is used
#'
#' @exportClass wtd
#' @importClassesFrom bbmle mle2
#'
setClass("wtd", contains="mle2", slots=c(delta="numeric", dist="character",
                                         depvar="character", idvar="character",
                                         isreverse="logical"))

