
# parse 'form' to determine the distribution in use and test if it
# is a supported one, otherwise error

# parse 'parameters' to test if they match 'form', otherwise error

# test if start, end are dates, error if a mix of types
# test if outcome variable is a date
# if start, end are dates and outcome is not, error

test_that("plausible density values", {
  expect_gte(min(dlnorm(df$obstime, 1.6, 3.4, 0.02)), 0)
  expect_lte(max(dlnorm(df$obstime, 1.6, 3.4, 0.02)), 1)
})
