# Test predict.wtd
# * all distributions DONE
# * date or continuous data DONE
# * forward or reverse DONE
# * different values of delta DONE
# * type=c("dur", "prob") DONE
# * iadmean=c(FALSE, TRUE) DONE
# * quantile=0.8, 0.5 DONE
# * distrx=NULL
# * prediction.data=NULL
# * se.fit=c(FALSE, TRUE)

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


  # Date data

  rd <- readRDS(test_path("fixtures", "randat_disc.rds"))

  x <- wtdttt(data = rd,
                 rxdate ~ dexp(logitp, lnbeta),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = FALSE
  )

  testthat::expect_equal(v(predict(x)), 68.41, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 29.46, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 42.50, tolerance=0.001)

#  BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(68.40503, 29.46044))),
#                         c(0.2, 0.5), tolerance=0.001)

  x <- wtdttt(data = rd,
                 rxdate ~ dweib(logitp, lnalpha, lnbeta),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = FALSE
  )

  testthat::expect_equal(v(predict(x)), 86.00, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 67.85, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 3.037, tolerance=0.001)

# BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(86.00535, 67.85363))),
#                         c(0.2, 0.5), tolerance=0.001)

  x <- wtdttt(data = rd,
                 rxdate ~ dlnorm(logitp, mu, lnsigma),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = FALSE
  )

  testthat::expect_equal(v(predict(x)), 85.06, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 68.81, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 71.03, tolerance=0.001)

#  BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(85.05506, 68.81494))),
#                         c(0.2, 0.5), tolerance=0.001)

  # reverse

  x <- wtdttt(data = rd,
                 rxdate ~ dexp(logitp, lnbeta),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = TRUE
  )

  testthat::expect_equal(v(predict(x)), 76.19, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 32.81, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 47.34, tolerance=0.001)

#  BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(76.18548, 32.8113))),
#                         c(0.2, 0.5), tolerance=0.001)

  x <- wtdttt(data = rd,
                 rxdate ~ dweib(logitp, lnalpha, lnbeta),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = TRUE
  )

  testthat::expect_equal(v(predict(x)), 88.73, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 70.17, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 3.030, tolerance=0.001)

#  BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(88.72821, 70.16795))),
#                         c(0.2, 0.5), tolerance=0.001)

  x <- wtdttt(data = rd,
                 rxdate ~ dlnorm(logitp, mu, lnsigma),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = TRUE
  )

  testthat::expect_equal(v(predict(x)), 87.96, tolerance=0.001)
  testthat::expect_equal(v(predict(x, quantile=0.5)), 69.80, tolerance=0.001)
  testthat::expect_equal(v(predict(x, iadmean=TRUE)), 72.49, tolerance=0.001)

#  BUG? As written function requires a date
#  testthat::expect_equal(v(predict(x, type="prob", distrx=c(87.96276, 69.79998))),
#                         c(0.2, 0.5), tolerance=0.001)

})
