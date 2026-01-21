# Simulate and Check Model Fitting with Litter Data

This internal function first fits a model to the `pineneedles` data from
the `litterfitter` package using the `fit_litter` function. Next, it
introduces random noise to the mass remaining data and refits the model
to this perturbed data. The function returns whether the time to
proportion mass remaining values from the two fits are within ten
percent of each other.

## Usage

``` r
simulate_and_check(model)
```

## Arguments

- model:

  A character string representing the model type to be fitted using
  `fit_litter`.

## Value

Logical. TRUE if the time to proportion mass remaining values from the
original and simulated fits are within ten percent of each other; FALSE
otherwise.

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md),
[`pineneedles`](http://traitecoevo.github.io/litterfitter/reference/pineneedles.md)
