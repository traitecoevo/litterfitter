# Create a bootstrap distribution of a particular coefficient from a model fit

Create a bootstrap distribution of a particular coefficient from a model
fit

## Usage

``` r
bootstrap_parameters(x,nboot,upper,lower,...)
```

## Arguments

- x:

  an object of class "`litfit`" (from
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md))
  or "`elementfit`" (from
  [`fit_element`](https://traitecoevo.github.io/litterfitter/reference/fit_element.md))

- nboot:

  number of bootstrap replications

- upper:

  optional vector of upper bounds for the bootstrap replicates

- lower:

  optional vector of lower bounds for the bootstrap replicates

- ...:

  passed to [`optim`](https://rdrr.io/r/stats/optim.html)

## Value

a `litfit_bootstrap` object: a matrix with one row per bootstrap
replicate and one column per parameter, plus derived quantities: the
implied steady state for a `litfit`, or the peak element remaining and
the mass remaining at the peak for an `elementfit`. Failed replicates
are recorded as `NA`.

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
)
#> Number of successful fits:  100  out of 100
boot1 <- bootstrap_parameters(fit, nboot = 500)
#> Warning: 11 of 500 bootstrap replicates failed and are recorded as NA
```
