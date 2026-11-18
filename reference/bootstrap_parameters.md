# Create a bootstrap distribution of a particular coefficient from a model fit

Create a bootstrap distribution of a particular coefficient from a model
fit

## Usage

``` r
bootstrap_parameters(x,nboot,upper,lower,...)
```

## Arguments

- x:

  an object of class "`litfit`"

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
replicate and one column per parameter plus the implied steady state.
Failed replicates are recorded as `NA`.

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
)
#> Number of successful fits:  100  out of 100
boot1 <- bootstrap_parameters(fit, nboot = 500)
```
