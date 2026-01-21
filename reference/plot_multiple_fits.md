# Plot multiple fits on one graph with model selection results displayed

Plot multiple fits of decomposition trajectories on one graph with model
selection results displayed

## Usage

``` r
plot_multiple_fits(time,mass.remaining,model,color,iters,bty,...)
```

## Arguments

- time:

  vector of time points

- mass.remaining:

  vector of mass remaining

- model:

  vector of models to fit and plot (see
  [`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md))

- color:

  a vector of colors the same length as the number of models

- iters:

  parameter passed to
  [`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)

- bty:

  bty

- ...:

  additional parameters passed to
  [`plot`](https://rdrr.io/r/graphics/plot.default.html)

## Value

plot of multiple fits, returns invisibly

## Details

this function is designed to compare a variety of curve shapes visually
and with AIC and BIC simultaneously

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
[`plot.litfit`](http://traitecoevo.github.io/litterfitter/reference/plot.litfit.md)

## Author

Liu Guofang

## Examples

``` r
data(pineneedles, package = "litterfitter")

plot_multiple_fits(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining,
  bty = "n", model = c("neg.exp", "weibull"),
  xlab = "Time", ylab = "Proportion mass remaining", iters = 200
)
#> Number of successful fits:  193  out of 200 
#> Number of successful fits:  200  out of 200 
#> Warning: May not have found global best fit; increase iterations

```
