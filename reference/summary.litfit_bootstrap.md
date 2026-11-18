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
#> Warning: 2 of 200 bootstrap replicates failed and are recorded as NA
summary(boot1)
#>                   mean      2.5%       50%     97.5%
#> k            0.2790202 0.2359009 0.2805891 0.3132986
#> steady_state 3.6048157 3.1918633 3.5639307 4.2392797
```
