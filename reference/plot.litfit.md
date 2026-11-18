# Plot a decomposition trajectory and its fitted curve

Plots the data and fitted curve from a `litfit` object, with the fitted
equation. If a bootstrap is supplied, a pointwise percentile band for
the curve is added.

## Usage

``` r
# S3 method for class 'litfit'
plot(x, formulae.cex = 1, boot = NULL, conf.level = 0.95, ...)
```

## Arguments

- x:

  a `litfit` object from
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)

- formulae.cex:

  size of the equation text; use `NA` to omit the equation

- boot:

  optional `litfit_bootstrap` object from
  [`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
  for the same fit, to draw an uncertainty band

- conf.level:

  confidence level for the band (default 0.95)

- ...:

  additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html), which
  override the defaults (e.g. `xlab`, `ylab`, `main`, `pch`)

## Value

`x`, invisibly

## See also

[`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md),
[`plot.litfit_list`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit_list.md);
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods for ggplot2 output
([`litterfitter_autoplot`](https://traitecoevo.github.io/litterfitter/reference/litterfitter_autoplot.md))

## Author

Will Cornwell

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  model = "weibull", iters = 100, quiet = TRUE
)
plot(fit)


boot <- bootstrap_parameters(fit, nboot = 100)
#> Warning: 1 of 100 bootstrap replicates failed and are recorded as NA
plot(fit, boot = boot, main = "Weibull fit with 95% band")

```
