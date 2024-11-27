# XXXX other test ideas for wtdttt()
# * test errors relating to start and end (types, values etc)
# * models with linear predictors on parameters
# * naming of parameters, including naming of parameter formulae
# * formatted output of print(), summary() [move to wtd-class testing?]
# * subset
# * init

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

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1), id=c(1)), form = rx1time ~ dpois(lambda),
           start=0, end=1, id="id"),
    "model must use one of dlnorm, dweib or dexp"
  )

})

testthat::test_that("preprocessing errors", {

  # FIXME preprocessing failure
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(2)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
  #   "All dates are out of the window"
  # )

  # FIXME preprocessing failure
  # testthat::expect_error(
  #   wtdttt(data.frame(rx1time=c(2), id=c(1)),
  #          form = rx1time ~ dlnorm(logitp, mu, lnsigma),
  #          id="id", start=0, end=1),
  #   "All dates are out of the window"
  # )

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  dt_exp$id <- seq_along(dt_exp$t)

  x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), id="id", start=0, end=1)

  testthat::expect_lte(max(x@data$t), 1)
  testthat::expect_gte(min(x@data$t), 0)

  tt <- setdiff(dt_exp$t, x@data$t) # XXXX this isn't quite right
  testthat::expect_true(all(tt < 0 | tt > 1))

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  # XXXX could check the log likelihood too

  testthat::expect_equal(as.vector(x@vcov), c(0.22448270, -0.08358377, -0.08358377, 0.08201046), tolerance=0.001)

  # now test when an id has multiple observations
  # check different scenarios, each one both forward and reverse

  orig_n <- length(x@data$t)
  dt_exp$tr <- 1 - dt_exp$t

  # 1. both within obs window, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.4), id=c(dt_exp$id, 99999, 99999)),
         form = t ~ dexp(logitp, lnbeta),
         id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 1. both within obs window, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 0.4), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$tr[x@data$id==99999], 1-0.4)

  # 1. both within obs window, reverse=F, duplicate
  x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.3), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 1. both within obs window, reverse=T, duplicate
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 0.3), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$tr[x@data$id==99999], 1-0.3)

  # 2. both greater than end, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, 1.1, 1.2), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 2. both greater than end, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, 1.1, 1.2), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 3. both less than begin, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, -0.1, -0.2), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 3. both less than begin, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, -0.1, -0.2), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 4. one greater, one less, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, -0.1, 1.1), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 4. one greater, one less, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, -0.1, 1.1), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 5. one within, one greater, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 1.1), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 5. one within, one greater, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 1.1), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 1-0.3)

  # 6. one within, one less, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, -0.1), id=c(dt_exp$id, 99999, 99999)),
              form = t ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 6. one within, one less, reverse=T
  x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, -0.1), id=c(dt_exp$id, 99999, 99999)),
              form = tr ~ dexp(logitp, lnbeta),
              id="id", start=0, end=1, reverse=T)

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 1-0.3)

  # Check that nothing happens to the data when preprocess=F

  dt <- data.frame(t=dt_exp$t[dt_exp$t >= 0 & dt_exp$t <= 1])
  dt$id <- seq_along(dt$t); dt$id <- dt$id %% 2 # only two different ids

  x <- wtdttt(dt, form = t ~ dexp(logitp, lnbeta), id="id", start=0, end=1, preprocess=F)

  testthat::expect_identical(x@data$t, dt$t)
  testthat::expect_identical(x@data$id, dt$id)

  # Quick check that alphanumeric ids work OK

  dt_exp$pid <- sprintf("A%05d", dt_exp$id)

  # both within obs window, reverse=F
  x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.4), pid=c(dt_exp$pid, "A99999", "A99999")),
              form = t ~ dexp(logitp, lnbeta),
              id="pid", start=0, end=1)

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$pid=="A99999"], 0.3)

})


testthat::test_that("warnings", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  testthat::expect_warning(
    wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
    "The id variable was not provided"
  )

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

# XXXX may make more sense to use the example data instead, but should convert it to rds first

testthat::test_that("basics", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_s4_class(x, "mle2")
  testthat::expect_s4_class(x, "wtd")

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  # XXXX could check the log likelihood too

  testthat::expect_equal(as.vector(x@vcov), c(0.22448270, -0.08358377, -0.08358377, 0.08201046), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dweib(logitp, lnalpha, lnbeta), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1304, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha"]), -0.2724, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.7721, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov),
                         c(0.3170, -0.07393, 0.04061, -0.07393, 0.1100,
                           -0.1823, 0.04061, -0.1823, 0.3759), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1040, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu"]), -2.754, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma"]), -0.1161, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.2965, -0.006223, 0.05308, -0.006223,
                                              0.1816, -0.08350,  0.05308, -0.08350,
                                              0.05993), tolerance=0.001)

  dt_exp$tr <- 1 - dt_exp$t

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = tr ~ dexp(logitp, lnbeta), start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

})
