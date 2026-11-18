# Tidy a bootstrap distribution

Returns the bootstrap replicates from
[`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
in long format, one row per replicate and parameter (including the
implied steady state). Failed replicates are dropped.

## Usage

``` r
# S3 method for class 'litfit_bootstrap'
tidy(x, ...)
```

## Arguments

- x:

  a `litfit_bootstrap` object

- ...:

  unused

## Value

A `data.frame` with columns `replicate`, `term` and `value`.

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  model = "neg.exp", iters = 50, quiet = TRUE
)
boot <- bootstrap_parameters(fit, nboot = 50)
head(tidy(boot))
#>   replicate term     value
#> 1         1    k 0.2743558
#> 2         2    k 0.2637692
#> 3         3    k 0.2846005
#> 4         4    k 0.2625795
#> 5         5    k 0.2846005
#> 6         6    k 0.2804885
```
