# Test sandwich estimation with linear predictors

testthat::test_that("sand_vcov", {

  rd <- readRDS(test_path("fixtures", "dt_sand.rds"))

  x <- wtdttt(data = rd,
                 rxdate ~ dexp(logitp, lnbeta),
                 parameters = list(logitp ~ logddd + sex),
                 id = "id",
                 start = as.Date('2019-01-01'),
                 end = as.Date('2019-12-31'),
                 reverse = TRUE,
                 preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(1.3589, -0.29219, -0.10844, 0.061022,
                                            -0.29219, 0.070556, -0.0027567, -0.019837,
                                            -0.10844, -0.0027567, 0.41332, -0.040013,
                                            0.061022, -0.019837, -0.040013, 0.048071), tolerance=0.001)

  x <- wtdttt(data = rd,
              rxdate ~ dexp(logitp, lnbeta),
              parameters = list(lnbeta ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(0.10848, -0.047162, 0.003902, 0.0096262,
                                            -0.047162, 0.93357, -0.19538, -0.093098,
                                            0.003902, -0.19538, 0.042679, 0.012478,
                                            0.0096262, -0.093098, 0.012478, 0.10722), tolerance=0.001)

  x <- wtdttt(data = rd,
              rxdate ~ dweib(logitp, lnalpha, lnbeta),
              parameters = list(logitp ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(1.3376, -0.28631, -0.10424, -0.069179, 0.12615,
                                            -0.28631, 0.068666, -0.0033899, 0.02058, -0.038535,
                                            -0.10424, -0.0033899, 0.40844, -0.010871, -0.027921,
                                            -0.069179, 0.02058, -0.010871, 0.1979, -0.24276,
                                            0.12615, -0.038535, -0.027921, -0.24276, 0.33309), tolerance=0.001)


  x <- wtdttt(data = rd,
              rxdate ~ dweib(logitp, lnalpha, lnbeta),
              parameters = list(lnalpha ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(0.10374, 0.03292, -0.0025923, -0.0050395, -0.041474,
                                            0.03292, 1.1644, -0.16353, -0.18515, -0.38125,
                                            -0.0025923, -0.16353, 0.034288, -0.0039358, 0.014998,
                                            -0.0050395, -0.18515, -0.0039358, 0.13348, 0.14783,
                                            -0.041474, -0.38125, 0.014998, 0.14783, 0.2826), tolerance=0.001)

  x <- wtdttt(data = rd,
              rxdate ~ dweib(logitp, lnalpha, lnbeta),
              parameters = list(lnbeta ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(0.097875, 0.016859, -0.052579, 0.0039012, 0.012879,
                                            0.016859, 0.54406, -0.55587, 0.04539, -0.047837,
                                            -0.052579, -0.55587, 1.2535, -0.19042, -0.06573,
                                            0.0039012, 0.04539, -0.19042, 0.035416, 0.013926,
                                            0.012879, -0.047837, -0.06573, 0.013926, 0.0828), tolerance=0.001)

  x <- wtdttt(data = rd,
              rxdate ~ dlnorm(logitp, mu, lnsigma),
              parameters = list(logitp ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(1.3696, -0.2948, -0.10362, -0.10761, 0.037867,
                                            -0.2948, 0.071501, -0.0039146, 0.038834, -0.013928,
                                            -0.10362, -0.0039146, 0.41629, 0.034015, 0.00091721,
                                            -0.10761, 0.038834, 0.034015, 0.22582, -0.10174,
                                            0.037867, -0.013928, 0.00091721, -0.10174, 0.053829), tolerance=0.001)

  x <- wtdttt(data = rd,
              rxdate ~ dlnorm(logitp, mu, lnsigma),
              parameters = list(mu ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  testthat::expect_equal(as.vector(vcov), c(0.11171, 0.28402, -0.034493, -0.01623, -0.14642,
                                            0.28402, 5.6226, -0.7214, 0.0067689, -3.3635,
                                            -0.034493, -0.7214, 0.10216, 0.0086894, 0.36722,
                                            -0.01623, 0.0067689, 0.0086894, 0.11015, -0.14031,
                                            -0.14642, -3.3635, 0.36722, -0.14031, 2.5205), tolerance=0.001)

  # this looks like it blew up

  x <- wtdttt(data = rd,
              rxdate ~ dlnorm(logitp, mu, lnsigma),
              parameters = list(lnsigma ~ logddd + sex),
              id = "id",
              start = as.Date('2019-01-01'),
              end = as.Date('2019-12-31'),
              reverse = TRUE,
              preprocess = FALSE
  )

  vcov <- Rwtdttt:::sand_vcov(x)

  # testthat::expect_equal(as.vector(vcov), c(0.078545, 0.0011788, 3.5972e-10, 9.7782e-10, 1.2142e-11,
  #                                           0.0011788, 2.1986e-05, 5.3991e-12, 1.4677e-11, 1.8228e-13,
  #                                           3.5972e-10, 5.3991e-12, 1.6474e-18, 4.4782e-18, 5.5609e-20,
  #                                           9.7782e-10, 1.4677e-11, 4.4782e-18, 1.2173e-17, 1.5116e-19,
  #                                           1.2142e-11, 1.8228e-13, 5.5609e-20, 1.5116e-19, 1.8771e-21), tolerance=0.001)

})

