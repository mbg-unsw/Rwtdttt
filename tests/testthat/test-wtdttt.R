# XXXX other test ideas for wtdttt()
# * pid works with alpha and numeric pids
# * preprocess works as expected
# * test errors relating to start and end (types, values etc)
# * naming of parameters, including naming of parameter formulae
# * formatted output of print(), summary() [move to wtd-class testing?]

testthat::test_that("errors", {

  testthat::expect_error(
    wtdttt(NULL, form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=0),
    "data must be non-empty"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=double()), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "data must be non-empty"
  )

  # FIXME no way to trigger this error?
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(1)), form = ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "obstime variable must be specified"
  # )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = foo ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "is not in data"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=as.Date("2023-01-01"), end=as.Date("2024-01-01")),
    "must be either all of class"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=as.Date("2024-01-01")), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "must be either all of class"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, subset=rx1time!=1),
    "data must be non-empty"
  )

  # FIXME is.na() check on line 153 is fragile
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id=c(1,2)),
  #   "id colname must be a single element"
  # )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id="foo"),
    "is not in data"
  )

  # FIXME preprocessing failure
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(2)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "All dates are out of the window"
  # )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dpois(lambda), start=0, end=1),
    "model must use one of dlnorm, dweib or dexp"
  )

})


testthat::test_that("warnings", {

  # testthat::expect_warning(
  #   wtdttt(2),
  #   "The id variable was not provided"
  # )
  #
  # # XXXX currently broken?
  # testthat::expect_warning(
  #   wtdttt(2),
  #   "Some dates are out of the window"
  # )

})

# simulate test data
# t <- c(runif(980, -1, 0) + rexp(980, rate=20), runif(30, max=1.5))
# dt_exp <- data.frame(t=t[t>-0.05])

# XXXX probably better to use the example data instead, but should convert it to rds first

testthat::test_that("basics", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1)

  testthat::expect_s4_class(x, "mle2")
  testthat::expect_s4_class(x, "wtd")

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.22448270, -0.08358377, -0.08358377, 0.08201046), tolerance=0.001)

})
