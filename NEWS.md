# litterfitter 0.2.0

## Behaviour changes

* `fit_litter()` now defaults to `model = "neg.exp"` when no model is given.
* `steady_state()` returns `Inf` for `neg.exp.limit` fits with B > 0 (previously a string).
* `time_to_prop_mass_remaining()` returns `NA` (with a warning) when the threshold is never
  reached, and its result is now exact rather than read off a grid, so values may differ slightly.
* `coef()` now returns a named vector.

## Bug fixes

* `fit_litter()` now defaults to `model = "neg.exp"` and rejects unknown model names
  (previously omitting `model` gave "first argument has length > 1").
* `fit_litter()` now rejects clearly negative mass, warns (rather than failing) on slightly
  negative values arising from measurement error (#21), drops missing observations with a
  warning, and requires more observations than model parameters.
* The "may not have found global best fit" check now compares the 5 *best* fits
  (it previously compared the 5 worst) and no longer errors when fewer than 5 starts succeed.
* `steady_state()` for `neg.exp.limit` now returns a number (`Inf` when B > 0) instead of a
  string, fixing errors in `summary()` and `bootstrap_parameters()` for that model.
* `bootstrap_parameters()` records failed replicates as `NA` instead of erroring.
* `time_to_prop_mass_remaining()` now uses `uniroot()`: exact, fast, independent of time
  units, and returns `NA` with a warning when the threshold is never reached.
* Plot curves use a fixed number of points regardless of time units; `plot_multiple_fits()`
  no longer clips mass values above 1.
* Fixed Weibull and discrete.series formula display in `plot.litfit()`.
* `plot.litfit()` always shows the (0, 1) starting point on the y axis (#7).
* All six models are now documented in `?fit_litter`; `neg.exp` had been accidentally
  commented out and `neg.exp.limit` was missing (#22).
* Restored input-validation tests that were lost in 0.1.5.

## New features

* New methods (#19): `print.litfit()`, `logLik.litfit()` (so `AIC(fit1, fit2)` and `BIC()` work),
  `nobs.litfit()`, `residuals.litfit()` and `summary.litfit_bootstrap()`.
* `coef()` and bootstrap output now carry parameter names.
* New `quiet` argument to `fit_litter()` suppresses the "successful fits" message.
* `plot_multiple_fits()` invisibly returns the fitted models.

## Performance and dependencies

* Model fitting is roughly 3-4x faster (objective function built once per fit).
* Dropped the `methods` dependency; removed unused `devtools`/`roxygen2` from Suggests.

# litterfitter 0.1.5

* fixing url in the readme, which is causing a note in the cran checks
* Modernized package structure and code style (styler, lintr)
* Updated GitHub Actions workflows for better CI/CD support (Ubuntu, macOS, Windows)
* Updated dependencies (R >= 4.1.0)

# litterfitter 0.1.4

* removed plyr dependency for long term ease of maintanance 
* updated testing framework to testthat v3 style
* fixed a testing instability that was causing occasional warnings
* updating doi syntax

# litterfitter 0.1.3

* updated package level documentation
* improved documentation for a number of functions 
* moving github home to traitecoevo

# litterfitter 0.1.2

* Added a `NEWS.md` file to track changes to the package.