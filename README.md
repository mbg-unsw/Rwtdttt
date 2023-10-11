# Rwtdttt
R package for WTD

Pharmacoepidemiologic studies based on information on dispensed medications typically require some definition of treatment duration after the dispensing. This duration - known as the prescription duration - is not recorded in the data recorded at the pharmacy at the time of dispensing, and so the duration must be defined or estimated. Most definitions are more or less arbitrary and rely on assumed dose per day, such as 1 pill per day or 1 Defined Daily Dose (DDD). Many studies have shown that such definitions have a poor agreement with actual drug usage patterns for many medications and consequently most pharmacoepidemiological studies will employ a range of sensitivity analyses based on variation of the used definition.

The parametric Waiting Time Distribution has been developed to provide a data-driven estimate of the prescription duration. It is a two-component mixture model which allows maximum likelihood estimation with inclusion of relevant covariates, prediction of individual probability of treatment at specific at time points. As the method relies on correct specification of a paraetric model, diagnostic plots may be used to inspect its validity.

This implementation of estimation procedures mimics that found in the corresponding Stata package (wtdttt). 
