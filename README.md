litterFitter
============

Package for fitting and testing alternative models for single cohort litter decomposition data

To install run:

```r
	install.packages("devtools")
	devtools::install_github("cornwell-lab-unsw/litterFitter")
	library(litterFitter)
```

At the moment there is only really one useful function with is `litterFitter`

Try:

```r
fit<-litterFitter(time=c(0,1,2,3,4,5,6),mass.remaining=c(1,0.9,1.01,0.4,0.6,0.2,0.01),model="weibull",iters=1000)
```

Then you can do some (limited) useful things with the `litFit` object like:

```r
plot(fit)
summary(fit)
```






