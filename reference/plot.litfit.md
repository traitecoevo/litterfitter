# Plot Decomposition Trajectory and Curve Fit

Plot a Litter Decomposition Trajectory with Curve Fit

## Usage

``` r
# S3 method for class 'litfit'
plot(x, formulae.cex = 1, ...)
```

## Arguments

- x:

  A `litfit` object.

- formulae.cex:

  Size scaling factor for the formula display on the plot.

- ...:

  Additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html).

## Value

A plot visualizing the data and curve fit from a `litfit` object. The
result is returned invisibly.

## Details

Visualizes the litter decomposition trajectory data and its curve fit
derived from a `litfit` object. This function is designed to provide a
quick visual check on the adequacy of model fitting.

The plot displays data points from the `litfit` object along with the
curve fit. The formula for the fit is displayed on the plot.

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
for generating `litfit` objects.

## Author

Will Cornwell

## Examples

``` r
fit <- fit_litter(
  time = c(0, 1, 2, 3, 4, 5, 6),
  mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
  "neg.exp",
  iters = 250
)
#> Number of successful fits:  246  out of 250 
plot(fit)

```
