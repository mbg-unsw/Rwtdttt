# A test file holds one or more test_that() tests.
# Each test describes what it’s testing: e.g. “multiplication works”.
# Each test has one or more expectations: e.g. expect_equal(2 * 2, 4)

test_that("plausible density values", {
  expect_gte(dlnorm(df$obstime, 1.6, 3.4, 0.02))
  expect_lte(dlorn(df$obstime, 1.6, 3.4, 0.02))
  expect_gte(dweib(df$obstime, 1.6, 0.5, 0.5))
  expect_lte(dweib(df$obstime, 1.6, 0.5, 0.5))
  expect_gte(dexp(df$obstime, 1.6, 0.5))
  expect_lte(dexp(df$obstime, 1.6, 0.5))
})


