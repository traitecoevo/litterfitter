# Fit and plot several models on one graph with model selection results

Fits several models to the same data with
[`fit_litter_all`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md)
and plots them together with
[`plot.litfit_list`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit_list.md).
Existing fits can be plotted instead by passing a `litfit_list` (or a
list of `litfit` objects) as the first argument, which avoids refitting.

## Usage

``` r
plot_multiple_fits(
  time,
  mass.remaining,
  model = c("neg.exp", "weibull", "discrete.parallel", "discrete.series", "cont.quality",
    "neg.exp.limit"),
  color = NULL,
  iters = 500,
  bty = "o",
  ...
)
```

## Arguments

- time:

  vector of time points, or a `litfit_list` / list of `litfit` objects
  to plot without refitting

- mass.remaining:

  vector of mass remaining (ignored when `time` is a list of fits)

- model:

  vector of models to fit and plot (see
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md))

- color:

  colours for the curves; defaults to the Okabe-Ito palette

- iters:

  number of random starts, passed to
  [`fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)

- bty:

  box type of the legend

- ...:

  additional arguments passed to
  [`plot.default`](https://rdrr.io/r/graphics/plot.default.html)

## Value

the fits, invisibly, as a `litfit_list`

## See also

[`fit_litter_all`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md),
[`plot.litfit_list`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit_list.md)

## Author

Liu Guofang

## Examples

``` r
plot_multiple_fits(
  time = pineneedles$Year,
  mass.remaining = pineneedles$Mass.remaining,
  bty = "n", model = c("neg.exp", "weibull"), iters = 200
)


# plot existing fits without refitting
fits <- fit_litter_all(pineneedles$Year, pineneedles$Mass.remaining,
  models = c("neg.exp", "cont.quality"), iters = 100
)
plot_multiple_fits(fits)

```
