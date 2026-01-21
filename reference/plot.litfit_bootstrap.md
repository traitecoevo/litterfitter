# Plot the bootstrap distribution for a parameter from a litfit object

Plot a bootstrap distribution of a particular coefficient

## Usage

``` r
# S3 method for class 'litfit_bootstrap'
plot(x,coef.index,bw,...)
```

## Arguments

- x:

  litfit object

- coef.index:

  coefficient number to plot from the `litfit` object, see order of
  coefficients for that particular model. Default is to plot the first
  parameter for that model

- bw:

  bandwidth (or bandwidth algorithm see
  [`density`](https://rdrr.io/r/stats/density.html)) for the density
  plot

- ...:

  additional arguments passed to plot.default

## Value

plot of litfit_bootstrap object, returns invisibly

## Details

The grey fill goes from 0.025 quantile to the 0.975 quantile of the
distribution. Red line shows the mean. Blue line shows the median.

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
[`bootstrap_parameters`](http://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
[`density`](https://rdrr.io/r/stats/density.html)

## Author

James Weedon

## Examples

``` r
fit <- fit_litter(time=pineneedles$Year,
mass.remaining=pineneedles$Mass.remaining, model='neg.exp', iters=200)
#> Number of successful fits:  194  out of 200 
boot1 <- bootstrap_parameters(fit, nboot = 500)
plot(boot1)

```
