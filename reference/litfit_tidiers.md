# Tidy, glance and augment methods for litfit objects

These methods return the results of a
[`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
fit as data frames, following the conventions of the broom package.

## Usage

``` r
# S3 method for class 'litfit'
tidy(x, boot = NULL, conf.level = 0.95, ...)

# S3 method for class 'litfit'
glance(x, ...)

# S3 method for class 'litfit'
augment(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...)
```

## Arguments

- x:

  a `litfit` object

- boot:

  optional `litfit_bootstrap` object from
  [`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
  for the same fit. In
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) it adds
  bootstrap standard errors and percentile intervals; in
  [`augment()`](https://generics.r-lib.org/reference/augment.html) it
  adds a pointwise percentile band for the fitted curve.

- conf.level:

  confidence level for intervals and bands (default 0.95)

- ...:

  unused

- newdata:

  optional numeric vector of times, or a data frame with a `time`
  column, at which to predict. If omitted, the original data are
  returned.

## Value

A `data.frame`.

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html):

  `term`, `estimate`; with `boot` also `std.error`, `conf.low`,
  `conf.high`.

- [`glance()`](https://generics.r-lib.org/reference/glance.html):

  `model`, `nobs`, `nparams`, `sigma` (residual standard deviation used
  in the likelihood), `logLik`, `AIC`, `AICc`, `BIC`, `converged`,
  `time_to_50`, `steady_state`. `time_to_50` is `NA` if the curve never
  reaches 50\\ `steady_state` can be `Inf` (see
  [`steady_state`](https://traitecoevo.github.io/litterfitter/reference/steady_state.md)).

- [`augment()`](https://generics.r-lib.org/reference/augment.html):

  without `newdata`: `time`, `mass`, `.fitted`, `.resid`. With
  `newdata`: the columns of `newdata` (or `time`) plus `.fitted`. With
  `boot`: also `.lower` and `.upper`.

## Details

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html): one row
  per parameter.

- [`glance()`](https://generics.r-lib.org/reference/glance.html): a
  one-row summary of the fit, including information criteria and the
  derived quantities time to 50\\

- [`augment()`](https://generics.r-lib.org/reference/augment.html): the
  data with fitted values and residuals, or predictions at new time
  points, optionally with a bootstrap uncertainty band.

## See also

[`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md),
[`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md),
[`fit_litter_all`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md)

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  model = "weibull", iters = 100, quiet = TRUE
)
tidy(fit)
#>    term estimate
#> 1  beta 3.520852
#> 2 alpha 1.092126
glance(fit)
#>     model nobs nparams      sigma   logLik       AIC      AICc       BIC
#> 1 weibull    6       2 0.05001515 9.458945 -14.91789 -10.91789 -15.33437
#>   converged time_to_50 steady_state
#> 1      TRUE   2.517099     3.405444
augment(fit)
#>        time    mass   .fitted       .resid
#> 1 0.3616438 0.97369 0.9200867  0.053603284
#> 2 0.8383562 0.84960 0.8116996  0.037900351
#> 3 1.8383562 0.58362 0.6115290 -0.027908981
#> 4 2.8356164 0.37805 0.4540819 -0.076031906
#> 5 3.8328767 0.34254 0.3338140  0.008725981
#> 6 4.8301370 0.30730 0.2435549  0.063745086

boot <- bootstrap_parameters(fit, nboot = 100)
tidy(fit, boot = boot)
#>    term estimate std.error  conf.low conf.high
#> 1  beta 3.520852 0.3378214 2.7415831  4.176179
#> 2 alpha 1.092126 0.1877691 0.7583242  1.602508
head(augment(fit, newdata = seq(0, 5, by = 0.5), boot = boot))
#>   time   .fitted    .lower    .upper
#> 1  0.0 1.0000000 1.0000000 1.0000000
#> 2  0.5 0.8881280 0.8022064 0.9374217
#> 3  1.0 0.7765261 0.6889024 0.8319860
#> 4  1.5 0.6744706 0.6024656 0.7396818
#> 5  2.0 0.5832113 0.5294072 0.6530491
#> 6  2.5 0.5025771 0.4201119 0.5758975
```
