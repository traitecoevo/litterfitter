# Changelog

## litterfitter 0.2.0 (GitHub only; not released to CRAN)

### Behaviour changes

- [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  now defaults to `model = "neg.exp"` when no model is given.
- [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  warns when mass remaining values are above 1 or below 0 (see Bug
  fixes).
- Fits can differ slightly from earlier versions because of the smaller
  optimiser step.
- [`steady_state()`](https://traitecoevo.github.io/litterfitter/reference/steady_state.md)
  returns `Inf` for `neg.exp.limit` fits with B \> 0 (previously a
  string).
- [`time_to_prop_mass_remaining()`](https://traitecoevo.github.io/litterfitter/reference/time_to_prop_mass_remaining.md)
  returns `NA` (with a warning) when the threshold is never reached, and
  its result is now exact rather than read off a grid, so values may
  differ slightly.
- [`coef()`](https://rdrr.io/r/stats/coef.html) now returns a named
  vector.

### Bug fixes

- [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  now defaults to `model = "neg.exp"` and rejects unknown model names
  (previously omitting `model` gave “first argument has length \> 1”).
- Proportional mass remaining values above 1 (apparent mass gain from
  measurement error, common early in decomposition or when decomposition
  is slow) are kept in the fit but now trigger an informative warning of
  class `litterfitter_mass_above_one`; when the data also show little
  overall mass loss the warning says that parameters may be poorly
  constrained and derived quantities are extrapolations
  ([\#21](https://github.com/traitecoevo/litterfitter/issues/21)).
  Slightly negative values (down to -0.1) are kept with a
  `litterfitter_mass_below_zero` warning; clearly negative values or
  percentages are errors.
- [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  now rejects infinite values, invalid `iters`, and data with a single
  time point; drops missing observations with a warning; and requires
  more observations than parameters.
- Boundary warnings now name the parameters on a bound, and hint that
  the default bounds assume time in years when time values are large.
- Fits are much more accurate when rates are small (e.g. time in days):
  the finite-difference step used by `optim` now defaults to
  `ndeps = 1e-4` (previously 1e-3); a user-supplied `control$ndeps`
  still takes precedence.
- The “may not have found global best fit” check now compares the 5
  *best* fits (it previously compared the 5 worst) and no longer errors
  when fewer than 5 starts succeed.
- [`steady_state()`](https://traitecoevo.github.io/litterfitter/reference/steady_state.md)
  for `neg.exp.limit` now returns a number (`Inf` when B \> 1e-4,
  otherwise A / k) instead of a string, fixing errors in
  [`summary()`](https://rdrr.io/r/base/summary.html) and
  [`bootstrap_parameters()`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md).
- [`bootstrap_parameters()`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
  records failed replicates as `NA` instead of erroring.
- [`time_to_prop_mass_remaining()`](https://traitecoevo.github.io/litterfitter/reference/time_to_prop_mass_remaining.md)
  now uses [`uniroot()`](https://rdrr.io/r/stats/uniroot.html): exact,
  fast, independent of time units, and returns `NA` with a warning when
  the threshold is never reached.
- Plot curves use a fixed number of points regardless of time units;
  [`plot_multiple_fits()`](https://traitecoevo.github.io/litterfitter/reference/plot_multiple_fits.md)
  no longer clips mass values above 1.
- Fixed Weibull and discrete.series formula display in
  [`plot.litfit()`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit.md).
- [`plot.litfit()`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit.md)
  always shows the (0, 1) starting point on the y axis
  ([\#7](https://github.com/traitecoevo/litterfitter/issues/7)).
- All six models are now documented in
  [`?fit_litter`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md);
  `neg.exp` had been accidentally commented out and `neg.exp.limit` was
  missing
  ([\#22](https://github.com/traitecoevo/litterfitter/issues/22)).
- [`time_to_prop_mass_remaining()`](https://traitecoevo.github.io/litterfitter/reference/time_to_prop_mass_remaining.md)
  gives a clear error for a missing `threshold.mass`.
- Restored input-validation tests that were lost in 0.1.5, and greatly
  expanded the test suite (edge cases for inputs, every model, derived
  quantities checked against closed-form results, bootstrap failures,
  methods and plots).

### New features

- New methods
  ([\#19](https://github.com/traitecoevo/litterfitter/issues/19)):
  `print.litfit()`,
  [`logLik.litfit()`](https://traitecoevo.github.io/litterfitter/reference/logLik.litfit.md)
  (so `AIC(fit1, fit2)` and [`BIC()`](https://rdrr.io/r/stats/AIC.html)
  work), `nobs.litfit()`, `residuals.litfit()` and
  [`summary.litfit_bootstrap()`](https://traitecoevo.github.io/litterfitter/reference/summary.litfit_bootstrap.md).
- [`coef()`](https://rdrr.io/r/stats/coef.html) and bootstrap output now
  carry parameter names.
- New `quiet` argument to
  [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  suppresses the “successful fits” message.
- [`plot_multiple_fits()`](https://traitecoevo.github.io/litterfitter/reference/plot_multiple_fits.md)
  invisibly returns the fitted models.

### Performance and dependencies

- Model fitting is roughly 3-4x faster (objective function built once
  per fit).
- Dropped the `methods` dependency; removed unused `devtools`/`roxygen2`
  from Suggests.

## litterfitter 0.1.5

- fixing url in the readme, which is causing a note in the cran checks
- Modernized package structure and code style (styler, lintr)
- Updated GitHub Actions workflows for better CI/CD support (Ubuntu,
  macOS, Windows)
- Updated dependencies (R \>= 4.1.0)

## litterfitter 0.1.4

CRAN release: 2025-08-21

- removed plyr dependency for long term ease of maintanance
- updated testing framework to testthat v3 style
- fixed a testing instability that was causing occasional warnings
- updating doi syntax

## litterfitter 0.1.3

CRAN release: 2023-08-29

- updated package level documentation
- improved documentation for a number of functions
- moving github home to traitecoevo

## litterfitter 0.1.2

CRAN release: 2022-09-15

- Added a `NEWS.md` file to track changes to the package.
