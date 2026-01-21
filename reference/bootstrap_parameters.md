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

litfit_bootstrap object

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
)
#> Number of successful fits:  99  out of 100 
boot1 <- bootstrap_parameters(fit, nboot = 500)
```
