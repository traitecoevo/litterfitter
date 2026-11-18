# Plot an element fit

Plots proportional element remaining against proportional mass
remaining, with mass remaining decreasing from left to right so that
decomposition proceeds along the x axis. A dashed line marks the initial
amount of the element.

## Usage

``` r
# S3 method for class 'elementfit'
plot(x, formulae.cex = 1, boot = NULL, conf.level = 0.95, ...)
```

## Arguments

- x:

  an `elementfit` object from
  [`fit_element`](https://traitecoevo.github.io/litterfitter/reference/fit_element.md)

- formulae.cex:

  size of the parameter text; use `NA` to omit it

- boot:

  optional `litfit_bootstrap` object for the same fit, to draw an
  uncertainty band

- conf.level:

  confidence level for the band (default 0.95)

- ...:

  additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

## Value

`x`, invisibly

## See also

[`fit_element`](https://traitecoevo.github.io/litterfitter/reference/fit_element.md)

## Examples

``` r
fit <- fit_element(
  mass.remaining = pineneedles$Mass.remaining,
  element.remaining = c(1.1, 1.2, 1.3, 1, 0.8, 0.6),
  iters = 200, quiet = TRUE
)
plot(fit)
```
