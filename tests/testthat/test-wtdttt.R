
# parse 'form' to determine the distribution in use and test if it
# is a supported one, otherwise error

# parse 'parameters' to test if they match 'form', otherwise error

# test if start, end are dates, error if a mix of types
# test if outcome variable is a date
# if start, end are dates and outcome is not, error

# check data.frame is not empty
test_that("data not empty", {
  expect_error(wtdttt(data = NULL), "data must be non-empty")
  # expect_error(wtdttt(data = NA), "data must be non-empty")
})

# check id.colname is not empty, single value, in data
test_that("id.colname not empty, single value, in data", {
  expect_error(wtdttt(data=df, id.colname = NULL), "id colname must be non-empty")
  expect_error(wtdttt(data=df, id.colname = c("pid","pp")), "id colname must be a single element")
  expect_error(wtdttt(data=df, id.colname = "ID"), "id colname is not in data")
})

# # test for expect_warning (not working)
# test_that("proportion of incidents is a positive value", {
#   expect_warning({delta <- as.double(as.Date('2014-12-31') - as.Date('2014-01-01'), units="days") + 1;
#                  ntot <- nrow(df);
#                  nonprevend <- sum(df$obstime > (delta * 2/3));
#                  prp <- 1 - 3 * nonprevend / ntot},
#                  "The proportion of incident users is a negative value")
# })

test_that("plausible density values", {

  for (logitp in -10:10) {
    for (mu in -10:10) {
      for (lnsigma in -10:10) {
        expect_gte(min(dlnorm(df$obstime, logitp, mu, lnsigma)), 0)
        expect_lte(max(dlnorm(df$obstime, logitp, mu, lnsigma)), 1)
      }
    }
  }
})


