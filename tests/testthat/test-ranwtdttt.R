# test ranwtdttt()
# * all tests from wtdttt()
# - plus to be done for wtdttt():
# - test errors relating to start and end (types, values etc)
# - naming of parameters, including naming of parameter formulae
# - formatted output of print(), summary() [move to wtd-class testing?]
# - subset
# - init

# * nsamp
# * robust

testthat::test_that("errors", {

  # FIXME generates the wrong error "some columns are not in the data.table"
  # testthat::expect_error(
  #   ranwtdttt(NULL, form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=0),
  #   "data must be non-empty"
  # )

  # FIXME generates the wrong error "some columns are not in the data.table"
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=double()), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "data must be non-empty"
  # )

  # FIXME no way to trigger this error?
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(1)), form = ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "obstime variable must be specified"
  # )

  # FIXME generates the wrong error "some columns are not in the data.table: [NA, foo]"
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1)), form = foo ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "is not in data"
  # )

  # FIXME should test that everything is a date
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=as.Date("2023-01-01"), end=as.Date("2024-01-01")),
  #   "must be either all of class"
  # )
  #
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=as.Date("2024-01-01")), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "must be either all of class"
  # )

  # FIXME generates the wrong error (bug in nrow(data) < 1 test)
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, subset=rx1time!=1),
  #   "data must be non-empty"
  # )

  # FIXME
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id=c(1,2)),
  #   "id colname must be a single element"
  # )

  # FIXME generates the wrong error "some columns are not in the data.table: [foo]
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id="foo"),
  #   "is not in data"
  # )

  # FIXME generates the wrong error (bug in if (dist == "lnorm") ...)
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(1), id=c(1)), form = rx1time ~ dpois(lambda),
  #          start=0, end=1, id="id"),
  #   "model must use one of dlnorm, dweib or dexp"
  # )

})

testthat::test_that("preprocessing errors", {

  # FIXME preprocessing failure
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(2)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "All dates are out of the window"
  # )

  # FIXME preprocessing failure
  # testthat::expect_error(
  #   ranwtdttt(data.frame(rx1time=c(2), id=c(1)),
  #          form = rx1time ~ dlnorm(logitp, mu, lnsigma),
  #          id="id", start=0, end=1),
  #   "All dates are out of the window"
  # )

  rd <- readRDS(test_path("fixtures", "randat_disc.rds"))
  npid <- length(unique(rd$pid))

  set.seed(127)

  # XXXX repeat all tests a few times to allow random variation in sampling?

  x <- ranwtdttt(data = rd,
                 rxdate ~ dlnorm(logitp, mu, lnsigma),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = FALSE,
                 robust = FALSE
  )

  testthat::expect_lte(max(x@data$rxshift), 365.5)
  testthat::expect_gte(min(x@data$rxshift), 0.5)
  testthat::expect_lte(length(x@data$pid), npid)
  testthat::expect_equal(length(x@data$pid), length(unique(x@data$pid)))

  # XXXX need to think about which obs are excluded so can test for them

  set.seed(127)
  x <- ranwtdttt(data = rd,
                 rxdate ~ dlnorm(logitp, mu, lnsigma),
                 id = "pid",
                 start = as.Date('2014-01-01'),
                 end = as.Date('2014-12-31'),
                 reverse = TRUE,
                 robust = FALSE
  )

  testthat::expect_lte(max(x@data$rxshift), 365.5)
  testthat::expect_gte(min(x@data$rxshift), 0.5)

  # now test when nsamp > 1
  # check different scenarios, each one both forward and reverse
  # XXXX

  # Quick check that alphanumeric ids work OK

  rd$apid <- sprintf("A%05d", rd$pid)

  # XXXX
})


testthat::test_that("warnings", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  # FIXME is an error rather than a warning, but generates the wrong one i.e.
  # "some columns are not in the data.table"
  # testthat::expect_warning(
  #   ranwtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
  #   "The id variable was not provided"
  # )

  # XXXX currently broken
  # testthat::expect_warning(
  #   wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
  #   "Some dates are out of the window"
  # )

  # dt_exp$id <- seq_along(dt_exp$t)
  #
  # testthat::expect_warning(
  #   wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), id="id", start=0, end=1),
  #   "Some dates are out of the window"
  # )

})

# simulate test data
# t <- c(runif(980, -1, 0) + rexp(980, rate=20), runif(30, max=1.5))
# dt_exp <- data.frame(t=t[t>-0.05])

# XXXX using a random sample of the example data instead, converted to rds

testthat::test_that("basics", {

  rd <- readRDS(test_path("fixtures", "randat_disc.rds"))

  set.seed(127)
  x <- ranwtdttt(data = rd,
                     rxdate ~ dlnorm(logitp, mu, lnsigma),
                     id = "pid",
                     start = as.Date('2014-01-01'),
                     end = as.Date('2014-12-31'),
                     reverse = TRUE,
                     robust = FALSE
  )

  testthat::expect_s4_class(x, "mle2")
  testthat::expect_s4_class(x, "wtd")

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1489, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu"]), 4.312, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma"]), -1.660, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 365)

  # XXXX could check the log likelihood too

  testthat::expect_equal(as.vector(x@vcov), c(0.0133589293, 0.0001877469, 0.0024136993,
                                              0.0001877469, 0.0012496759, -0.004001219,
                                              0.0024136993, -0.0040012191, 0.0283231302), tolerance=0.001)

  # testthat::expect_warning(
  #   x <- wtdttt(dt_exp, form = t ~ dweib(logitp, lnalpha, lnbeta), start=0, end=1),
  #   "The id variable was not provided"
  # )
  #
  # testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1304, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["lnalpha"]), -0.2724, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.7721, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)
  #
  # testthat::expect_equal(as.vector(x@vcov),
  #                        c(0.3170, -0.07393, 0.04061, -0.07393, 0.1100,
  #                          -0.1823, 0.04061, -0.1823, 0.3759), tolerance=0.001)

  # x <- ranwtdttt(dt_exp, form = t ~ dlnorm(logitp, mu, lnsigma), start=0, end=1)
  #
  # testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1040, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["mu"]), -2.754, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["lnsigma"]), -0.1161, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)
  #
  # testthat::expect_equal(as.vector(x@vcov), c(0.2965, -0.006223, 0.05308, -0.006223,
  #                                             0.1816, -0.08350,  0.05308, -0.08350,
  #                                             0.05993), tolerance=0.001)

  # dt_exp$tr <- 1 - dt_exp$t

  # x <- wtdttt(dt_exp, form = tr ~ dexp(logitp, lnbeta), start=0, end=1, reverse=T)
  #
  # testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  # testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

})
