#' summary.wtd-class - R class definition
#'
#' Register a 'summary.wtd' class, inheriting from 'summary.mle2'
#'
#' @slot call call of the model formula
#' @slot coef matrix of coefficients estimated through wtdttt function
#' @slot m2logL log-likelihood
#' @slot prev_fin estimate of prevalence of drug use and its 95% confidence interval
#'
#' @exportClass summary.wtd
#' @importClassesFrom bbmle mle2 summary.mle2
#' @importMethodsFrom bbmle summary
#'
setClass("summary.wtd",
         contains = "summary.mle2",
         slots = list(call = "call",
                      coef = "matrix",
                      m2logL = "numeric",
                      prev_fin = "data.frame"))
