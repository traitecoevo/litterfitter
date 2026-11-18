# Plot bootstrap distributions

Plots the density of the bootstrap distribution of one or more
parameters (or derived quantities) from
[`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md).

## Usage

``` r
# S3 method for class 'litfit_bootstrap'
plot(x, coef.index = 1, bw = "nrd0", conf.level = 0.95, ...)
```

## Arguments

- x:

  a `litfit_bootstrap` object

- coef.index:

  which column(s) to plot: a number, a column name such as `"k"` or
  `"steady_state"`, a vector of either, or `"all"`. Several columns are
  drawn in a grid of panels. With `"all"`, columns without at least two
  finite values (e.g. an always-infinite steady state) are skipped with
  a warning.

- bw:

  bandwidth (or bandwidth rule, see
  [`density`](https://rdrr.io/r/stats/density.html)) for the density

- conf.level:

  width of the shaded central interval (default 0.95)

- ...:

  additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

## Value

`x`, invisibly

## Details

The grey fill covers the central `conf.level` interval of the
distribution. The red dashed line shows the mean and the blue dashed
line the median. Non-finite values (e.g. an infinite steady state) and
failed replicates are ignored.

## See also

[`bootstrap_parameters`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md),
[`density`](https://rdrr.io/r/stats/density.html)

## Author

James Weedon

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  model = "weibull", iters = 100, quiet = TRUE
)
boot <- bootstrap_parameters(fit, nboot = 200)
#> Warning: 1 of 200 bootstrap replicates failed and are recorded as NA
plot(boot)
plot(boot, coef.index = "all")

```
