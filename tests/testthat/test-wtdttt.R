# XXXX other test ideas for wtdttt()
# * test errors relating to start and end (types, values etc)
# * models with linear predictors on parameters
# * naming of parameters, including naming of parameter formulae
# * formatted output of print(), summary() [move to wtd-class testing?]
# * subset
# * init

testthat::test_that("warnings", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  testthat::expect_warning(
    testthat::expect_warning(
      wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
      "The id variable was not provided"
    ),
    "Some dates are out of the window"
  )

  dt_exp$id <- seq_along(dt_exp$t)

  testthat::expect_warning(
    wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), id="id", start=0, end=1),
    "Some dates are out of the window"
  )

})

testthat::test_that("errors", {

  testthat::expect_error(
    wtdttt(NULL, form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=0),
    "data must be non-empty"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=double()), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "data must be non-empty"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(1)), form = ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "obstime variable must be specified"
  )

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

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(2)), form = rx1time ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
    "All dates are out of the window"
  )

  testthat::expect_error(
    wtdttt(data.frame(rx1time=c(2), id=c(1)),
           form = rx1time ~ dlnorm(logitp, mu, lnsigma),
           id="id", start=0, end=1),
    "All dates are out of the window"
  )

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  dt_exp$id <- seq_along(dt_exp$t)

  testthat::expect_warning(
    x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), id="id", start=0, end=1),
    "Some dates are out of the window"
  )

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
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.4), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 1. both within obs window, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 0.4), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$tr[x@data$id==99999], 1-0.4)

  # 1. both within obs window, reverse=F, duplicate
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.3), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 1. both within obs window, reverse=T, duplicate
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 0.3), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$tr[x@data$id==99999], 1-0.3)

  # 2. both greater than end, reverse=F
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 1.1, 1.2), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 2. both greater than end, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, 1.1, 1.2), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 3. both less than begin, reverse=F
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, -0.1, -0.2), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 3. both less than begin, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, -0.1, -0.2), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 4. one greater, one less, reverse=F
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, -0.1, 1.1), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 4. one greater, one less, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, -0.1, 1.1), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n)
  testthat::expect_true(all(x@data$id!=99999))

  # 5. one within, one greater, reverse=F
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 1.1), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 5. one within, one greater, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, 1.1), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$tr, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 1-0.3)

  # 6. one within, one less, reverse=F
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, -0.1), id=c(dt_exp$id, 99999, 99999)),
                form = t ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$id==99999], 0.3)

  # 6. one within, one less, reverse=T
  testthat::expect_warning(
    x <- wtdttt(data.frame(tr=c(dt_exp$tr, 0.3, -0.1), id=c(dt_exp$id, 99999, 99999)),
                form = tr ~ dexp(logitp, lnbeta),
                id="id", start=0, end=1, reverse=T),
    "Some dates are out of the window"
  )

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
  testthat::expect_warning(
    x <- wtdttt(data.frame(t=c(dt_exp$t, 0.3, 0.4), pid=c(dt_exp$pid, "A99999", "A99999")),
                form = t ~ dexp(logitp, lnbeta),
                id="pid", start=0, end=1),
    "Some dates are out of the window"
  )

  testthat::expect_length(x@data$t, orig_n+1)
  testthat::expect_equal(x@data$t[x@data$pid=="A99999"], 0.3)

})


# simulate test data
# t <- c(runif(980, -1, 0) + rexp(980, rate=20), runif(30, max=1.5))
# dt_exp <- data.frame(t=t[t>-0.05])

# XXXX may make more sense to use the example data instead, but should convert it to rds first

testthat::test_that("basics", {

  dt_exp <- readRDS(test_path("fixtures", "dt_exp.rds"))

  testthat::expect_warning(
    testthat::expect_warning(
      x <- wtdttt(dt_exp, form = t ~ dexp(logitp, lnbeta), start=0, end=1),
      "The id variable was not provided"
    ),
    "Some dates are out of the window"
  )

  testthat::expect_s4_class(x, "mle2")
  testthat::expect_s4_class(x, "wtd")

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  # XXXX could check the log likelihood too

  testthat::expect_equal(as.vector(x@vcov), c(0.22448270, -0.08358377, -0.08358377, 0.08201046), tolerance=0.001)

  testthat::expect_warning(
    testthat::expect_warning(
      x <- wtdttt(dt_exp, form = t ~ dweib(logitp, lnalpha, lnbeta), start=0, end=1),
      "The id variable was not provided"
    ),
    "Some dates are out of the window"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.1304, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha"]), -0.2724, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.7721, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov),
                         c(0.3170, -0.07393, 0.04061, -0.07393, 0.1100,
                           -0.1823, 0.04061, -0.1823, 0.3759), tolerance=0.001)

  testthat::expect_warning(
    testthat::expect_warning(
      x <- wtdttt(dt_exp, form = t ~ dlnorm(logitp, mu, lnsigma), start=0, end=1),
      "The id variable was not provided"
    ),
    "Some dates are out of the window"
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
    testthat::expect_warning(
      x <- wtdttt(dt_exp, form = tr ~ dexp(logitp, lnbeta), start=0, end=1, reverse=T),
      "The id variable was not provided"
    ),
    "Some dates are out of the window"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 0.92326, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 2.44031, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

})

# test linear predictors using a 1000 obs subsample of wtddat_covar.dta
# with added df2$sex = sample(c("F","M"), dim(df2)[1], replace = T)

# 1) dexp with same factor on each param and then both
# 2) repeat for dlnorm and dweib
# 3) try various combinations of two variables, just for dlnorm

testthat::test_that("linear predictors", {

  dt_coef <- readRDS(test_path("fixtures", "dt_coef.rds"))

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dexp(logitp, lnbeta),
                parameters = list(logitp ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.714, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.1026, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 1.994, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0355100761, -0.0308891057, -0.0027668640,
                                              -0.0308891057, 0.0747244970, -0.0008563311,
                                              -0.0027668640, -0.0008563311,  0.0021694335), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dexp(logitp, lnbeta),
                parameters = list(lnbeta ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.(Intercept)"]), 2.179, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.factor(packsize)200"]), -0.3078, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.707, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0201519378, -0.0030786659, 0.0008980731,
                                              -0.0030786659, 0.0042464681, -0.0039133330,
                                              0.0008980731, -0.0039133330, 0.0069563018), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dexp(logitp, lnbeta),
                parameters = list(logitp ~ factor(packsize), lnbeta ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.480, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.6531, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.(Intercept)"]), 2.212, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.factor(packsize)200"]), -0.3873, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.028022640, -0.028022640, -0.003951351, 0.003951351,
                                              -0.028022640, 0.109100059, 0.003951351, -0.012768115,
                                              -0.003951351, 0.003951351, 0.004240779, -0.004240779,
                                              0.003951351, -0.012768115, -0.004240779, 0.008281612), tolerance=0.001)

  # repeat for dweib

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dweib(logitp, lnalpha, lnbeta),
                parameters = list(logitp ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.403, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.0859, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha"]), 0.8515, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 1.502, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0200617567, -0.0187506809, -0.0016739867, 0.0000166038,
                                              -0.0187506809, 0.0470408256, -0.0014638976, -0.0007170799,
                                              -0.0016739867, -0.0014638976, 0.0119874009, -0.0036179635,
                                              0.0000166038, -0.0007170799, -0.0036179635, 0.0020142174), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dweib(logitp, lnalpha, lnbeta),
                parameters = list(lnalpha ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.426, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha.(Intercept)"]), 1.106, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha.factor(packsize)200"]), -0.577, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta"]), 1.579, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0122517683, -0.0022377929, 0.0012614296, -0.0004757542,
                                              -0.0022377929, 0.0176373667, -0.0127015418, -0.0027491634,
                                              0.0012614296, -0.0127015418, 0.0186612577, -0.0007173674,
                                              -0.0004757542, -0.0027491634, -0.0007173674, 0.0021075616), tolerance=0.001)
  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dweib(logitp, lnalpha, lnbeta),
                parameters = list(lnbeta ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.408, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha"]), 1.055, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.(Intercept)"]), 1.670, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.factor(packsize)200"]), -0.3675, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0118619059, -0.0025086392, -0.0005427019, 0.0004326068,
                                              -0.0025086392, 0.0133559795, -0.0024420993, -0.0005059701,
                                              -0.0005427019, -0.0024420993, 0.0021303557, -0.0015329506,
                                              0.0004326068, -0.0005059701, -0.0015329506, 0.0028725341), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dweib(logitp, lnalpha, lnbeta),
                parameters = list(lnalpha ~ factor(packsize), lnbeta ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.417, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha.(Intercept)"]), 0.9144, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnalpha.factor(packsize)200"]), 0.3395, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.(Intercept)"]), 1.702, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnbeta.factor(packsize)200"]), -0.4400, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(1.175113e-02, -2.626085e-03, 8.568624e-04, -2.763873e-04, -6.435163e-05,
                                              -2.626085e-03, 2.118410e-02, -2.078875e-02, -5.310449e-03, 5.386592e-03,
                                              8.568624e-04, -2.078875e-02, 5.424961e-02, 5.352079e-03, -1.133926e-02,
                                              -2.763873e-04, -5.310449e-03, 5.352079e-03, 3.063863e-03, -3.055847e-03,
                                              -6.435163e-05, 5.386592e-03, -1.133926e-02, -3.055847e-03, 5.197126e-03), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(logitp ~ factor(packsize), lnsigma ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.264, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.5048, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu"]), -1.700, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.(Intercept)"]), -1.278, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.factor(packsize)200"]), 0.6230, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0200595927, -0.0203365425, 0.0005999629, 0.0047830158, -0.0056157550,
                                              -0.0203365425, 0.0670622643,  -0.0014850197, -0.0036915099, 0.0125659207,
                                              0.0005999629, -0.0014850197, 0.0019173190,  -0.0023645545, -0.0002966548,
                                              0.0047830158, -0.0036915099, -0.0023645545, 0.0167589969,  -0.0134770317,
                                              -0.0056157550, 0.0125659207, -0.0002966548, -0.0134770317, 0.0185724071), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(logitp ~ factor(packsize), mu ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.215, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.5050, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.(Intercept)"]), -1.802, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.factor(packsize)200"]), 0.4224, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma"]), -1.192, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0196561877, -0.0188255696, 0.0008023386, -0.0017475723, 0.0034490154,
                                              -0.0188255696, 0.0500916185, -0.0013677386, 0.0031712619, -0.0006471799,
                                              0.0008023386, -0.0013677386, 0.0021096659, -0.0014662473, -0.0023477375,
                                              -0.0017475723, 0.0031712619, -0.0014662473, 0.0029927471, -0.0008407188,
                                              0.0034490154, -0.0006471799, -0.0023477375, -0.0008407188, 0.0116341966), tolerance=0.001)
  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(mu ~ factor(packsize), lnsigma ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.435, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.(Intercept)"]), -1.821, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.factor(packsize)200"]), 0.4713, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.(Intercept)"]), -1.053, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.factor(packsize)200"]), -0.3135, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(1.242591e-02, -1.767488e-05, 1.616132e-04, 3.247926e-03, -9.156832e-04,
                                              -1.767488e-05, 2.956697e-03, -2.956901e-03, -4.740316e-03, 4.736999e-03,
                                              1.616132e-04, -2.956901e-03, 5.701084e-03, 4.777939e-03, -1.174682e-02,
                                              3.247926e-03, -4.740316e-03, 4.777939e-03, 1.727121e-02, -1.666160e-02,
                                              -9.156832e-04, 4.736999e-03, -1.174682e-02, -1.666160e-02, 4.646623e-02), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(mu ~ factor(packsize), lnsigma ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp"]), 1.435, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.(Intercept)"]), -1.821, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.factor(packsize)200"]), 0.4713, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.(Intercept)"]), -1.053, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.factor(packsize)200"]), -0.3135, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(1.242591e-02, -1.767488e-05, 1.616132e-04, 3.247926e-03, -9.156832e-04,
                                              -1.767488e-05, 2.956697e-03, -2.956901e-03, -4.740316e-03, 4.736999e-03,
                                              1.616132e-04, -2.956901e-03, 5.701084e-03, 4.777939e-03, -1.174682e-02,
                                              3.247926e-03, -4.740316e-03, 4.777939e-03, 1.727121e-02, -1.666160e-02,
                                              -9.156832e-04, 4.736999e-03, -1.174682e-02, -1.666160e-02, 4.646623e-02), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(logitp ~ factor(packsize), mu ~ factor(packsize), lnsigma ~ factor(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.241, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(packsize)200"]), 0.4507, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.(Intercept)"]), -1.821, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.factor(packsize)200"]), 0.4735, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.(Intercept)"]), -1.104, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma.factor(packsize)200"]), -0.2123, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.0205612812, -0.0205612811, 0.0001719277, -0.0001719277, 0.0054325754, -0.0054325754,
                                              -0.0205612811, 0.0522465014, -0.0001719274, 0.0001612630, -0.0054325758, 0.0119608588,
                                              0.0001719277, -0.0001719274, 0.0027866940, -0.0027866935, -0.0045211253, 0.0045211240,
                                              -0.0001719277, 0.0001612630, -0.0027866935, 0.0055943737, 0.0045211244, -0.0114625569,
                                              0.0054325754, -0.0054325758, -0.0045211253, 0.0045211244, 0.0187078482, -0.0187078461,
                                              -0.0054325754, 0.0119608588, 0.0045211240, -0.0114625569, -0.0187078461, 0.0482691875), tolerance=0.001)

  testthat::expect_warning(
    x <- wtdttt(dt_coef, form = last_rxtime ~ dlnorm(logitp, mu, lnsigma),
                parameters = list(logitp ~ factor(sex), mu ~ factor(sex) + log(packsize)),
                start=0, end=1, reverse=T),
    "The id variable was not provided"
  )

  testthat::expect_equal(as.vector(x@fullcoef["logitp.(Intercept)"]), 1.3800, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["logitp.factor(sex)M"]), 0.0972, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.(Intercept)"]), -4.361, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.factor(sex)M"]), -0.007194, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["mu.log(packsize)"]), 0.5592, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["lnsigma"]), -1.172, tolerance=0.001)
  testthat::expect_equal(as.vector(x@fullcoef["delta"]), 1)

  testthat::expect_equal(as.vector(x@vcov), c(0.021889, -0.021186, 0.0032169, -0.0012657, -0.00052144, 0.0021901,
                                              -0.021186, 0.047538, -0.0015232, 0.0032528, 2.034e-05, 0.001362,
                                              0.0032169, -0.0015232, 0.14837, -0.0033776, -0.029515, 0.0030948,
                                              -0.0012657, 0.0032528, -0.0033776, 0.0031273, 0.00044929, -0.00070277,
                                              -0.00052144, 2.034e-05, -0.029515, 0.00044929, 0.0059464, -0.0011311,
                                              0.0021901, 0.001362, 0.0030948, -0.00070277, -0.0011311, 0.011191), tolerance=0.001)

})

