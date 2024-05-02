# A test file holds one or more test_that() tests.
# Each test describes what it’s testing: e.g. “multiplication works”.
# Each test has one or more expectations: e.g. expect_equal(2 * 2, 4)

# dummy tests for now

set.seed(42)
x <- runif(100)

test_that("plausible density values", {
  expect_true(all(dlnorm(x, 1.6, 3.4, 0.02, 1) > 0))
  expect_true(all(dlnorm(x, 1.6, 3.4, 0.02, 1) < 1))
  expect_true(all(dweib(x, 1.6, 0.5, 0.5, 1) > 0))
  expect_true(all(dweib(x, 1.6, 0.5, 0.5, 1) < 2))
  expect_true(all(dexp(x, 1.6, 0.5, 1) > 0))
  expect_true(all(dexp(x, 1.6, 0.5, 1) < 2))
})


