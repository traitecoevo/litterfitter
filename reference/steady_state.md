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
  "discrete.series", "cont.quality" or "neg.exp.limit". Only needed if
  `x` is not provided.

## Value

A named numeric value representing the estimated steady state biomass
from the specified model.

## Details

Computes the steady state biomass, as a proportion of the annual input,
based on a given model fit or parameters.

For `neg.exp.limit`, a fraction \\B\\ of the litter never decomposes, so
the implied steady state is infinite whenever \\B \> 0\\. Because \\B\\
is bounded just above zero during fitting, values of \\B \< 10^{-4}\\
are treated as zero and the steady state is \\A / k\\.

## See also

[`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
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
#> Warning: 1 of 7 mass remaining values are above 1 (max 1.01), i.e. apparent mass gain. They are kept in the fit and treated as measurement error.
#> Number of successful fits:  250  out of 250
steady_state(fit)
#> neg.exp 
#> 4.60503 

# Example with specific model and parameter values
steady_state(pars = c(6, 2), model = "weibull")
#>  weibull 
#> 5.317362 
```
