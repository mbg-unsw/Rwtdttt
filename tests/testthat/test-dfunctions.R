# A test file holds one or more test_that() tests.
# Each test describes what it’s testing: e.g. “multiplication works”.
# Each test has one or more expectations: e.g. expect_equal(2 * 2, 4)

#####################################################################

# do we have default values for parameters?
test_that("dnorm() on default values", {
  # obtain the output values to be used
  d <- dlnorm(x = df$obstime,
             logitp = 1.6,
             mu = 3.4,
             lnsigma = 0.02)

  # expect_is(d, "numeric")
  expect_type(d, "double") # test if d is a numeric with decimals (double)
  expect_equal(length(d), dim(df)[1]) # test if length of density function vector is equal to the number of rows of the data.frame
})

test_that("dweib() on default values", {
  # obtain the output values to be used
  d <- dweib(x = df$obstime,
              logitp = 1.6,
              lnalpha = 3.4,
              lnbeta = 0.02)

  # expect_is(d, "numeric")
  expect_type(d, "double") # test if d is a numeric with decimals (double)
  expect_equal(length(d), dim(df)[1]) # test if length of density function vector is equal to the number of rows of the data.frame
})

test_that("dexp() on default values", {
  # obtain the output values to be used
  d <- dexp(x = df$obstime,
              logitp = 1.6,
              lnbeta = 0.02)

  # expect_is(d, "numeric")
  expect_type(d, "double") # test if d is a numeric with decimals (double)
  expect_equal(length(d), dim(df)[1]) # test if length of density function vector is equal to the number of rows of the data.frame
})


test_that("plausible density values", {
  expect_gte(min(dlnorm(df$obstime, 1.6, 3.4, 0.02)), 0)
  expect_lte(max(dlnorm(df$obstime, 1.6, 3.4, 0.02)), 1)
  expect_gte(min(dweib(df$obstime, 1.6, 0.5, 0.5)), 0)
  expect_lte(max(dweib(df$obstime, 1.6, 0.5, 0.5)), 1)
  expect_gte(min(dexp(df$obstime, 1.6, 0.5)), 0)
  expect_lte(max(dexp(df$obstime, 1.6, 0.5)), 1)
})


