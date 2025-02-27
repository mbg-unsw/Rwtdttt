#' Summarise model object and prevalence of drug use
#'
#' Summarise model object from wtdttt and print the estimated prevalence of drug use along
#' with its 95% confidence interval .
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A summary of the fitted object
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
            prev <- round(inv.logit(coef_value), 7)

            lower_ci_prev <- round(inv.logit(coef_value - qnorm(0.975) * sqrt(vcov_value)), 7)

            upper_ci_prev <- round(inv.logit(coef_value + qnorm(0.975) * sqrt(vcov_value)), 7)

            prev_fin <- data.frame(Estimate = prev, Lower.95 = lower_ci_prev, Upper.95 = upper_ci_prev, row.names = "prevalence")


            new("summary.wtd", mle_summary, prev_fin = prev_fin)

          })


#' Formatting of the summary of the model object and the prevalence of drug use
#'
#' @param object a fitted object of class inheriting from "wtd"
#'
#' @returns A summary of the fitted object along with the estimated prevalence and its 95% CI
#' @export
#'
#' @examples
setMethod("show", "summary.wtd",
          function(object) {

            callNextMethod()

            cat("\n")
            print(object@prev_fin)

          })
