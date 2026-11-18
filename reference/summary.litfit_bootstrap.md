# Summarise a bootstrap distribution of parameters from a litfit object

Summarise a bootstrap distribution of parameters from a litfit object

## Usage

``` r
# S3 method for class 'litfit_bootstrap'
summary(object, probs = c(0.025, 0.5, 0.975), ...)
```

## Arguments

- object:

  a `litfit_bootstrap` object from
  [`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)

- probs:

  quantiles to report (default gives a 95 percent interval and the
  median)

- ...:

  unused

## Value

a matrix with one row per parameter (plus steady state) giving the mean
and the requested quantiles of the bootstrap distribution, ignoring
failed replicates

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining, model = "neg.exp", iters = 100
)
#> Number of successful fits:  100  out of 100
boot1 <- bootstrap_parameters(fit, nboot = 200)
summary(boot1)
#>                   mean      2.5%       50%     97.5%
#> k            0.2790103 0.2362142 0.2802469 0.3131491
#> steady_state 3.6047357 3.1933743 3.5682853 4.2335203
```
