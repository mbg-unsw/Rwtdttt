# Rwtdttt

Rwtdttt is an R package to estimate parameters of the ordinary or reverse waiting time distribution (WTD) for applications in pharmacoepidemiology and drug utilisation research.

## Installation

You can install `Rwtdttt` from `github` using the `devtools` package

```r
require(devtools)
install_github('mbg-unsw/Rwtdttt')
```

## Description

Pharmacoepidemiologic studies based on information on dispensed medications typically require some definition of treatment duration after the dispensing. This duration - known as the prescription duration - is not recorded in the data recorded at the pharmacy at the time of dispensing, and so the duration must be defined or estimated. Most definitions are more or less arbitrary and rely on assumed dose per day, such as 1 pill per day or 1 Defined Daily Dose (DDD). Many studies have shown that such definitions have a poor agreement with actual drug usage patterns for many medications and consequently most pharmacoepidemiological studies will employ a range of sensitivity analyses based on variation of the used definition.

The parametric Waiting Time Distribution has been developed to provide a data-driven estimate of the prescription duration. It is a two-component mixture model which allows maximum likelihood estimation with inclusion of relevant covariates, prediction of individual probability of treatment at specific at time points. As the method relies on correct specification of a paraetric model, diagnostic plots may be used to inspect its validity.

This implementation of estimation procedures mimics that found in the corresponding Stata package (wtdttt). The model is fit using maximum likelihood estimation.

## More

### Known bugs

Robust variance calculation, used by default in the randwtdttt() function, is very slow. We are working on improving this.

### Bug reports, feature requests etc.

This is a young project with some rough edges. Please submit any bug reports, feature request and comments as a Github issue.

Contributions are welcome. Please open an issue or submit a PR for review.

### Credits

The package was developed by:

* Sabrina Giometto
* Henrik Støvring
* Malcolm Gillies

### License

Rwtdttt is licensed under the GNU General Public License, Version 3.

