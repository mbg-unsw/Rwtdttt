
# make sure class definition is listed first in DESCRIPTION Collate:
#' @include summary_wtd-class.R
NULL

#' Extract coefficients
#'
#' Extract the coefficients from an object of class inheriting from "wtd"
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A summary of the fitted object
#' @export
#'
#' @examples
setMethod("coef", "wtd", function(object) { object@coef })

#' Extract variance-covariance matrix
#'
#' Extract variance-covariance matrix from an object of class inheriting from "wtd"
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A vector of coefficients
#' @export
#'
#' @examples
setMethod("vcov", "wtd", function(object) { object@vcov })

#' Summarise model object and prevalence of drug use
#'
#' Summarise model object from wtdttt and print the estimated prevalence of drug use along
#' with its 95% confidence interval .
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A summary of the fitted object
#' @importFrom stats plogis
#' @importFrom methods new
#' @export
#'
#' @examples
setMethod("summary", "wtd",
          function(object) {

            # base summary of the model
            mle_summary <- summary(as(object, "mle2"))


            if("logitp.(Intercept)" %in% names(coef(object))) {

              coef_value <- coef(object)["logitp.(Intercept)"]
              vcov_value <- vcov(object)["logitp.(Intercept)", "logitp.(Intercept)"]

            } else {

              coef_value <- coef(object)["logitp"]
              vcov_value <- vcov(object)["logitp", "logitp"]

            }

            # compute prevalence and its 95% confidence interval
            prev <- round(plogis(coef_value), 7)

            lower_ci_prev <- round(plogis(coef_value - qnorm(0.975) * sqrt(vcov_value)), 7)

            upper_ci_prev <- round(plogis(coef_value + qnorm(0.975) * sqrt(vcov_value)), 7)
            # propagation error formula: SE(p) = partial derivative of invlogit * SE(logit)
            se_prev <- round(prev*(1-prev)*sqrt(vcov_value), 7)
            z_value <- round(prev/se_prev, 7)
            p_value <- round(2*pnorm(z_value, lower.tail = F), 7)

            prev_fin <- data.frame(Estimate = prev, Std.Error = se_prev, z_value = z_value, p_value = p_value, Lower.95 = lower_ci_prev, Upper.95 = upper_ci_prev, row.names = "prevalence")

            new("summary.wtd", mle_summary, prev_fin = prev_fin)

          })


#' Formatting of the summary of the model object and the prevalence of drug use
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A summary of the fitted object along with the estimated prevalence and its 95% CI
#' @importFrom methods callNextMethod
#' @export
#'
#' @examples
setMethod("show", "summary.wtd",
          function(object) {

            callNextMethod()

            cat("\n")
            colnames(object@prev_fin) <- c("Estimate", "Std. Error", "z value", "Pr(z)", "Lower.95", "Upper.95")
            print(object@prev_fin)


          })
