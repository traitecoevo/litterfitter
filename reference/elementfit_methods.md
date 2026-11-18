# Methods for elementfit objects

Methods for element fits from
[`fit_element`](https://traitecoevo.github.io/litterfitter/reference/fit_element.md).
The predictor is proportional mass remaining, so `newdata` gives mass
remaining values.

## Usage

``` r
# S3 method for class 'elementfit'
print(x, ...)

# S3 method for class 'elementfit'
plot(x, ...)

# S3 method for class 'elementfit'
coef(object, ...)

# S3 method for class 'elementfit'
fitted(object, ...)

# S3 method for class 'elementfit'
residuals(object, ...)

# S3 method for class 'elementfit'
nobs(object, ...)

# S3 method for class 'elementfit'
logLik(object, ...)

# S3 method for class 'elementfit'
predict(object, newdata = NULL, ...)

# S3 method for class 'elementfit'
tidy(x, boot = NULL, conf.level = 0.95, ...)

# S3 method for class 'elementfit'
glance(x, ...)

# S3 method for class 'elementfit'
augment(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...)
```

## Arguments

- x, object:

  an `elementfit` object

- ...:

  further arguments; for
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

- newdata:

  optional numeric vector of mass remaining values, or (for
  [`augment()`](https://generics.r-lib.org/reference/augment.html)) a
  data frame with a `mass` column

- boot:

  optional `litfit_bootstrap` object from
  [`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
  for the same fit

- conf.level:

  confidence level for intervals and bands

## Value

[`tidy()`](https://generics.r-lib.org/reference/tidy.html),
[`glance()`](https://generics.r-lib.org/reference/glance.html) and
[`augment()`](https://generics.r-lib.org/reference/augment.html) return
a `data.frame` (see
[`litfit_tidiers`](https://traitecoevo.github.io/litterfitter/reference/litfit_tidiers.md)
for the shared columns;
[`augment()`](https://generics.r-lib.org/reference/augment.html) uses
`mass` and `element` columns).
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`print()`](https://rdrr.io/r/base/print.html) return `x` invisibly.

## Details

[`glance()`](https://generics.r-lib.org/reference/glance.html) reports,
besides information criteria, the peak proportional element remaining
(`peak_element`) and the mass remaining at which it occurs
(`mass_at_peak`). A peak of 1 at a mass of 1 means no net
immobilisation.

## Examples

``` r
fit <- fit_element(
  mass.remaining = pineneedles$Mass.remaining,
  element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
  iters = 200, quiet = TRUE
)
coef(fit)
#>         a         b 
#> 0.4919647 0.6456967 
predict(fit, newdata = c(1, 0.75, 0.5, 0.25))
#> [1] 1.0000000 1.2980816 1.2015235 0.5550716
tidy(fit)
#>   term  estimate
#> 1    a 0.4919647
#> 2    b 0.6456967
glance(fit)
#>          model nobs nparams      sigma   logLik       AIC      AICc       BIC
#> 1 import.model    6       2 0.06808585 7.608284 -11.21657 -7.216568 -11.63305
#>   converged peak_element mass_at_peak
#> 1      TRUE     1.357312    0.6456784
head(augment(fit, newdata = seq(1, 0, by = -0.1)))
#>   mass  .fitted
#> 1  1.0 1.000000
#> 2  0.9 1.118508
#> 3  0.8 1.242870
#> 4  0.7 1.339351
#> 5  0.6 1.342427
#> 6  0.5 1.201523
```
