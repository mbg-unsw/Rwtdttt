# wtd-class - R class definition

# Register a 'wtd' class, inheriting from 'mle2'

#' @slot delta
#' @slot dist
#' @slot depvar
#' @slot idvar
#'
#' @exportClass wtd
#' @importClassesFrom bbmle mle2
#'
setClass("wtd", contains="mle2", slots=c(delta="numeric", dist="character",
                                         depvar="character", idvar="character"))

