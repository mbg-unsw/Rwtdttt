* Test sandwich variance estimates with linear predictors on parameters
* Results to be compared with equivalent R output, see sand_test.R

use "sand_test_data.dta"
generate ldd = ln(ddd)
encode sex, gen(nsex)
wtdttt rxshift, start('01jan2014') end('31dec2014') reverse vce cluster(id) disttype(lnorm) mucovar(ldd) lnsigmacovar(i.nsex)
wtdttt rxshift, start('01jan2014') end('31dec2014') reverse vce cluster(id) disttype(lnorm) logitpcovar(i.nsex)
wtdttt rxshift, start('01jan2014') end('31dec2014') reverse vce cluster(id) disttype(lnorm) logitpcovar(i.nsex) mucovar(ldd)
wtdttt rxshift, start('01jan2014') end('31dec2014') reverse vce cluster(id) disttype(lnorm) logitpcovar(i.nsex) mucovar(ldd) lnsigmacovar(ldd)
