# Plot several fits to the same data

Plots the data with the fitted curve of every model in a `litfit_list`,
and a legend table of AIC, AICc and BIC sorted by AICc.

## Usage

``` r
# S3 method for class 'litfit_list'
plot(
  x,
  col = NULL,
  legend.pos = "topright",
  legend.bty = "o",
  legend.cex = 0.8,
  ...
)
```

## Arguments

- x:

  a `litfit_list` from
  [`fit_litter_all`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md)
  or
  [`as_litfit_list`](https://traitecoevo.github.io/litterfitter/reference/as_litfit_list.md)

- col:

  colours for the curves, recycled; defaults to the colour-blind safe
  Okabe-Ito palette

- legend.pos:

  position of the legend, passed to
  [`legend`](https://rdrr.io/r/graphics/legend.html); use `NA` to omit
  it

- legend.bty:

  box type of the legend (`"o"` or `"n"`)

- legend.cex:

  size of the legend text

- ...:

  additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

## Value

`x`, invisibly

## Details

Data points are taken from the first fit; fits in a `litfit_list` are
normally made to the same data.

## See also

[`plot_multiple_fits`](https://traitecoevo.github.io/litterfitter/reference/plot_multiple_fits.md),
[`glance.litfit_list`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)

## Examples

``` r
fits <- fit_litter_all(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  models = c("neg.exp", "weibull", "cont.quality"), iters = 100
)
plot(fits)
```
