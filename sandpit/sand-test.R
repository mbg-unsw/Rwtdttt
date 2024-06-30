# Test sandwich variance estimates with linear predictors on parameters
# Results to be compared with equivalent Stata output, see sand_test.do

# devtools::load_all(".")
# library(haven)
# library(bbmle)

df <- haven::read_dta(system.file("extdata", "sand_test_data.dta", package="Rwtdttt"))

fit1 <- wtdttt(data=df, rxshift ~ dlnorm(logitp, mu, lnsigma),
  id="id", start = as.Date('2014-01-01'), end = as.Date('2014-12-31'), reverse=T,
  parameters=list(mu~log(ddd), lnsigma~as.factor(sex)), preprocess=F)
summary(fit1)
sqrt(diag(sand_vcov(fit1)))

fit1 <- wtdttt(data=df, rxshift ~ dlnorm(logitp, mu, lnsigma),
  id="id", start = as.Date('2014-01-01'), end = as.Date('2014-12-31'), reverse=T,
  parameters=list(logitp~as.factor(sex)), preprocess=F)
summary(fit1)
sqrt(diag(sand_vcov(fit1)))

fit1 <- wtdttt(data=df, rxshift ~ dlnorm(logitp, mu, lnsigma),
  id="id", start = as.Date('2014-01-01'), end = as.Date('2014-12-31'), reverse=T,
  parameters=list(logitp~as.factor(sex), mu~log(ddd)), preprocess=F)
summary(fit1)
sqrt(diag(sand_vcov(fit1)))

fit1 <- wtdttt(data=df, rxshift ~ dlnorm(logitp, mu, lnsigma),
  id="id", start = as.Date('2014-01-01'), end = as.Date('2014-12-31'), reverse=T,
  parameters=list(logitp~as.factor(sex), mu~log(ddd), lnsigma~log(ddd)), preprocess=F)
summary(fit1)
sqrt(diag(sand_vcov(fit1)))
