# Fit several decomposition models to the same data

Fits each of the requested models with
[`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
and returns them together as a `litfit_list`, ready for model comparison
with
[`glance`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md),
[`tidy`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
or
[`augment`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md).

## Usage

``` r
fit_litter_all(
  time,
  mass.remaining,
  models = c("neg.exp", "weibull", "discrete.parallel", "discrete.series",
    "cont.quality", "neg.exp.limit"),
  iters = 500,
  ...
)
```

## Arguments

- time:

  time since decomposition began

- mass.remaining:

  proportional mass remaining

- models:

  models to fit (see
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md));
  defaults to all six

- iters:

  number of random starts for each model

- ...:

  further arguments passed to
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md),
  such as `upper`, `lower` or optim `control`

## Value

A `litfit_list`: a named list of `litfit` objects.

## Details

Warnings about the data (for example values above 1, or missing values
removed) are given once rather than once per model. Warnings specific to
one model, such as parameters on a boundary, are prefixed with the model
name. A model that cannot be fitted (for example, too few observations
for its number of parameters) is dropped with a warning; an error is
raised only if no model can be fitted.

## See also

[`as_litfit_list`](https://traitecoevo.github.io/litterfitter/reference/as_litfit_list.md)
to combine existing fits,
[`litfit_list_methods`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)

## Examples

``` r
fits <- fit_litter_all(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  models = c("neg.exp", "weibull", "cont.quality"), iters = 100
)
#> Warning: cont.quality: one or more parameters fit on the boundary (a), check fit closely
fits
#> litfit_list: 3 models fit to 6 observations
#> 
#>         model nparams logLik    AICc delta_AICc
#>       neg.exp       1  9.197 -15.395      0.000
#>       weibull       2  9.459 -10.918      4.477
#>  cont.quality       2  9.197 -10.394      5.001
glance(fits)
#>            fit        model nobs nparams      sigma   logLik       AIC
#> 1      neg.exp      neg.exp    6       1 0.05224361 9.197395 -16.39479
#> 2      weibull      weibull    6       2 0.05001515 9.458945 -14.91789
#> 3 cont.quality cont.quality    6       2 0.05224822 9.196866 -14.39373
#>        AICc       BIC converged time_to_50 steady_state delta_AICc
#> 1 -15.39479 -16.60303      TRUE   2.493138     3.596837   0.000000
#> 2 -10.91789 -15.33437      TRUE   2.517099     3.405444   4.476900
#> 3 -10.39373 -14.81021      TRUE   2.490919     3.617370   5.001059
```
