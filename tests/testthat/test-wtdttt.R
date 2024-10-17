# XXXX other test ideas for wtdttt()
# 1. return value is of class wtd-class and inherits from mle
# 2. pid works with alpha and numeric pids
# 3, test errors relating to start and end (types, values etc)
# 4. naming of parameters, including naming of parameter formulae

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
    wtdttt(data.frame(rx1time=as.Date("2024-01-01"))), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1,
    "must be either all of class"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, subset=rx1time!=1),
    "data must be non-empty"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id=c(1,2)),
    "id colname must be a single element"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1, id="foo"),
    "is not in data"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(2)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "All dates are out of the window"
  )

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
