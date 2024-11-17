# Test density functions dexp, dweib, dlnorm
# TODO
# * check density=0 if x<0
# * check more values of lnalpha, lnbeta, lnsigma, delta

test_that("identities", {
  expect_equal(dexp(0, 0, 0, log=FALSE), 1)
  expect_equal(dexp(0, 0, 0, log=TRUE), 0)
  expect_equal(dexp(0, 0, 0, delta=2, log=FALSE), 0.75)
  expect_equal(dexp(c(0,1), 100, 0, log=FALSE), c(1, exp(-1)))
  expect_equal(dexp(c(0,1), 100, 0, log=TRUE), c(0, -1))
  expect_equal(dexp(c(0,1), -100, 0, log=FALSE), c(1, 1))
  expect_equal(dexp(c(0,1), -100, 0, log=TRUE), c(0, 0))
  expect_equal(dexp(1, 100, log(2), log=FALSE), 2*exp(-2))

  set.seed(42)
  x <- runif(100)

  expect_equal(dweib(x, 0, 0, 1), dexp(x, 0, 1))
  expect_equal(dweib(x, 0, 0, 1, log=TRUE), dexp(x, 0, 1, log=TRUE))

  expect_equal(dlnorm(x, 100, -0.5, 0), pnorm(log(x), -0.5, 1, lower.tail=F))
  expect_equal(dlnorm(x, 100, -0.5, 0, log=TRUE),
               pnorm(log(x), -0.5, 1, lower.tail=F, log.p=TRUE))
})
