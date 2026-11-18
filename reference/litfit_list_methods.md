# Methods for litfit_list objects

Model comparison tools for several fits to the same data, as returned by
[`fit_litter_all`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md)
or
[`as_litfit_list`](https://traitecoevo.github.io/litterfitter/reference/as_litfit_list.md).

## Usage

``` r
# S3 method for class 'litfit_list'
print(x, ...)

# S3 method for class 'litfit_list'
glance(x, ...)

# S3 method for class 'litfit_list'
tidy(x, ...)

# S3 method for class 'litfit_list'
augment(x, newdata = NULL, boot = NULL, conf.level = 0.95, ...)

# S3 method for class 'litfit_list'
coef(object, ...)

# S3 method for class 'litfit_list'
x[i]
```

## Arguments

- x:

  a `litfit_list`

- ...:

  unused

- newdata:

  optional times at which to predict (see
  [`augment.litfit`](https://traitecoevo.github.io/litterfitter/reference/litfit_tidiers.md))

- boot:

  optional named list of `litfit_bootstrap` objects, one per fit (names
  matching the fits), to add uncertainty bands

- conf.level:

  confidence level for bands (default 0.95)

- object:

  a `litfit_list`

- i:

  indices or names of fits to keep

## Value

[`glance()`](https://generics.r-lib.org/reference/glance.html),
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`augment()`](https://generics.r-lib.org/reference/augment.html) return
a `data.frame`; [`coef()`](https://rdrr.io/r/stats/coef.html) returns a
named list of parameter vectors; `[` returns a `litfit_list`.

## Details

- [`glance()`](https://generics.r-lib.org/reference/glance.html): one
  row per fit, sorted by AICc, with `delta_AICc` (difference from the
  best model).

- [`tidy()`](https://generics.r-lib.org/reference/tidy.html): parameter
  estimates for every fit.

- [`augment()`](https://generics.r-lib.org/reference/augment.html):
  fitted values (or predictions at `newdata`) for every fit, stacked,
  for plotting.

Every table starts with a `fit` column holding the names of the list.

## Examples

``` r
fits <- fit_litter_all(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  models = c("neg.exp", "weibull"), iters = 100
)
glance(fits)
#>       fit   model nobs nparams      sigma   logLik       AIC      AICc
#> 1 neg.exp neg.exp    6       1 0.05224361 9.197395 -16.39479 -15.39479
#> 2 weibull weibull    6       2 0.05001515 9.458945 -14.91789 -10.91789
#>         BIC converged time_to_50 steady_state delta_AICc
#> 1 -16.60303      TRUE   2.493138     3.596837     0.0000
#> 2 -15.33437      TRUE   2.517099     3.405444     4.4769
tidy(fits)
#>       fit   model  term estimate
#> 1 neg.exp neg.exp     k 0.278022
#> 2 weibull weibull  beta 3.520852
#> 3 weibull weibull alpha 1.092126
head(augment(fits, newdata = seq(0, 5, by = 0.5)))
#>       fit   model time   .fitted
#> 1 neg.exp neg.exp  0.0 1.0000000
#> 2 neg.exp neg.exp  0.5 0.8702184
#> 3 neg.exp neg.exp  1.0 0.7572801
#> 4 neg.exp neg.exp  1.5 0.6589991
#> 5 neg.exp neg.exp  2.0 0.5734732
#> 6 neg.exp neg.exp  2.5 0.4990470
coef(fits)
#> $neg.exp
#>        k 
#> 0.278022 
#> 
#> $weibull
#>     beta    alpha 
#> 3.520852 1.092126 
#> 
fits["weibull"]
#> litfit_list: 1 model fit to 6 observations
#> 
#>    model nparams logLik    AICc delta_AICc
#>  weibull       2  9.459 -10.918          0
```
