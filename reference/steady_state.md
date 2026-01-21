# Estimate Steady State Biomass

Estimate Steady State Biomass

## Usage

``` r
steady_state(x = NULL, pars = NULL, model = NULL)
```

## Arguments

- x:

  A `litfit` object. If provided, `pars` and `model` parameters are
  extracted from this object.

- pars:

  A numeric vector of parameters for the model. Only needed if `x` is
  not provided.

- model:

  A character string specifying the decomposition model. Must be one of
  the following: "neg.exp", "weibull", "discrete.parallel",
  "discrete.series", or "cont.quality2". Only needed if `x` is not
  provided.

## Value

A named numeric value representing the estimated steady state biomass
from the specified model.

## Details

Computes the steady state biomass, as a proportion of the annual input,
based on a given model fit or parameters.

Currently, the function supports a subset of decomposition models. New
model support is planned for future updates.

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
for generating `litfit` objects.

## Author

Will Cornwell

## Examples

``` r
# Example with litfit object
fit <- fit_litter(
  time = c(0, 1, 2, 3, 4, 5, 6),
  mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
  model = "neg.exp",
  iters = 250
)
#> Number of successful fits:  248  out of 250 
steady_state(fit)
#> neg.exp 
#> 4.60503 

# Example with specific model and parameter values
steady_state(pars = c(6, 2), model = "weibull")
#>  weibull 
#> 5.317362 
```
