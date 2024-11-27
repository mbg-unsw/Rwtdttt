# Test predict.wtd
# * all distributions
# * date or continuous data
# * forward or reverse
# * different values of delta
# * prediction.data=NULL
# * type=c("dur", "prob")
# * iadmean=c(FALSE, TRUE)
# * distrx=NULL
# * quantile=0.8
# * se.fit=c(FALSE, TRUE)

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

testthat::test_that("errors", {

  # stop("Covariates used in the estimation are not in the prediction dataset (new data)")

})

v <- function(x) as.vector(x, mode="numeric")

testthat::test_that("predictions", {

  # durations, simple
  # continuous data

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1402, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.06040, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.08713, tolerance=0.001)

  # BUG
  #testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.1402369, 0.06039674))),
  #                       c(0.2, 0.5), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dweib(logitp, lnalpha, lnbeta), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1168, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.03864, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.03087, tolerance=0.001)

  testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.1168112, 0.03864412))),
                         c(0.2, 0.5), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1348, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.06367, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.09468, tolerance=0.001)

  # BUG
  #testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.13476, 0.06369762))),
  #                       c(0.2, 0.5), tolerance=0.001)

  # And for reverse...

  dt_exp$tr <- 1 - dt_exp$t

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = tr ~ dexp(logitp, lnbeta), start=0, end=1, reverse=TRUE),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1402, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.06040, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.08713, tolerance=0.001)

  # BUG
  #testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.1402369, 0.06039674))),
  #                       c(0.2, 0.5), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = tr ~ dweib(logitp, lnalpha, lnbeta), start=0, end=1, reverse=TRUE),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1168, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.03864, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.03087, tolerance=0.001)

#  BUG
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.1168112, 0.03864412))),
#                         c(0.2, 0.5), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = tr ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, reverse=TRUE),
    "The id variable was not provided"
  )

  testthat::expect_equal(v(predict(x)), 0.1348, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 0.06367, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 0.09468, tolerance=0.001)

  # BUG
  #testthat::expect_equal(v(predict(x, type="prob", distrx=c(0.13476, 0.06369762))),
  #                       c(0.2, 0.5), tolerance=0.001)


})
