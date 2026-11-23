# litterfitter 0.3.1

* Made two tests robust to platform-dependent optimiser behaviour reported by CRAN
  (r-oldrel-macos, BLIS, noLD and linux-arm64 checks). No changes to package code.

# litterfitter 0.3.0

This is the first CRAN release since 0.1.4. It also includes all changes from 0.1.5 and
0.2.0 (listed below), which were not released to CRAN.

## New features

* New `tidy()`, `glance()` and `augment()` methods (from the `generics` package, compatible
  with `broom`) return fits as data frames (#29):
  * `tidy()`: parameter estimates, with bootstrap standard errors and percentile intervals
    when a `bootstrap_parameters()` result is supplied.
  * `glance()`: a one-row summary with information criteria, time to 50% mass loss and
    steady state.
  * `augment()`: data with fitted values and residuals, or predictions at new times, with an
    optional bootstrap uncertainty band (`.lower`, `.upper`).
  * `tidy()` for bootstrap output gives the replicates in long format.
* New `fit_litter_all()` fits several models to the same data and returns a `litfit_list`;
  `as_litfit_list()` combines existing fits. `glance()` on a `litfit_list` gives a model
  comparison table sorted by AICc with `delta_AICc`; `tidy()`, `augment()`, `coef()`,
  `print()` and `[` also work on it.
* New `fit_element()` fits element (nutrient) dynamics: proportional element remaining
  (e.g. N/N0) as a function of proportional mass remaining, using the Parton et al. (2007)
  import model (reparameterised for proportional mass). It returns an `elementfit` object with
  `print`, `plot`, `coef`, `fitted`, `residuals`, `predict`, `nobs`, `logLik`, `tidy`, `glance`
  (including the peak element remaining and the mass at the peak) and `augment` methods, and
  is supported by `bootstrap_parameters()`. Based on work by Baptiste Wijas on the `import`
  branch (#30).
* Plotting has been modernised (#28):
  * New `autoplot()` methods (ggplot2, in Suggests) for `litfit`, `elementfit`,
    `litfit_list` and `litfit_bootstrap` objects, with optional bootstrap uncertainty
    bands, the fitted equation as subtitle, models labelled by their difference in AICc,
    and bootstrap distributions of all parameters in facets.
  * `plot()` for `litfit` and `elementfit` objects accepts `boot` to draw an uncertainty
    band; user arguments such as `xlab` or `main` override the defaults; the equation is
    placed in a corner so it no longer overlaps the data.
  * New `plot()` method for `litfit_list` with a colour-blind safe (Okabe-Ito) palette and
    an aligned table of AIC, AICc and BIC.
  * `plot_multiple_fits()` keeps its interface but now uses `fit_litter_all()` and the
    new `plot()` method, returns a `litfit_list`, and can plot existing fits without
    refitting.
  * `plot()` on a bootstrap can show several columns by number or name, or all of them
    (`coef.index = "all"`), and the shaded interval is set by `conf.level`.
  * Base plots now look slightly different (palette, legend layout, equation position).
* The missing-value warning from `fit_litter()` now has class `litterfitter_missing_removed`.
* New dependencies: `generics` and `utils` (Imports); `ggplot2` and `vdiffr` (Suggests).

## Behaviour changes

* `fit_litter()` again fits a single harvest, or replicates at a single time,
  for models whose curve starts at mass 1 at time 0 (all except
  `neg.exp.limit`), as 0.1.x did. The unreleased 0.2.0 rejected these data, which broke
  workflows that fit `neg.exp` k for each species and site. A fit is now
  allowed when there are at least as many distinct harvest times after
  time 0 as parameters. Fits that are possible only because of this rule
  give a once-per-session warning of class `litterfitter_origin_only`.
* AICc is `NA` when it is undefined (fewer than the number of parameters
  plus two observations), instead of a value with a zero or negative
  denominator.

# litterfitter 0.2.0 (GitHub only; not released to CRAN)


## Behaviour changes

* `fit_litter()` now defaults to `model = "neg.exp"` when no model is given.
* `fit_litter()` warns when mass remaining values are above 1 or below 0 (see Bug fixes).
* Fits can differ slightly from earlier versions because of the smaller optimiser step.
* `steady_state()` returns `Inf` for `neg.exp.limit` fits with B > 0 (previously a string).
* `time_to_prop_mass_remaining()` returns `NA` (with a warning) when the threshold is never
  reached, and its result is now exact rather than read off a grid, so values may differ slightly.
* `coef()` now returns a named vector.

## Bug fixes

* `fit_litter()` now defaults to `model = "neg.exp"` and rejects unknown model names
  (previously omitting `model` gave "first argument has length > 1").
* Proportional mass remaining values above 1 (apparent mass gain from measurement error, common
  early in decomposition or when decomposition is slow) are kept in the fit but now trigger an
  informative warning of class `litterfitter_mass_above_one`; when the data also show little
  overall mass loss the warning says that parameters may be poorly constrained and derived
  quantities are extrapolations (#21). Slightly negative values (down to -0.1) are kept with a
  `litterfitter_mass_below_zero` warning; clearly negative values or percentages are errors.
* `fit_litter()` now rejects infinite values, invalid `iters`, and data with a single time point;
  drops missing observations with a warning; and requires more observations than parameters.
* Boundary warnings now name the parameters on a bound, and hint that the default bounds assume
  time in years when time values are large.
* Litter fits and their bootstrap replicates use analytic gradients for all six
  curves (#27), improving accuracy for small rates such as rates per day.
  `discrete.series` now supports equal pool rates. Element fits and calls with
  `gr = NULL` use finite differences (default `ndeps = 1e-4`). Built-in
  gradients use a default limit of 500 optimizer iterations so that
  `cont.quality` fits can converge along a shallow minimum. An explicit
  `control$maxit` takes precedence.
  In a local benchmark on `pineneedles` (median of three fixed-seed runs),
  the 200-start `discrete.series` fit took 0.078 s versus 0.109 s previously,
  and 200 bootstrap replicates took 0.053 s versus 0.080 s.
* Weibull fits and bootstraps require a strictly positive lower bound for the
  scale parameter `beta`. Bootstrap replicates that reach the optimizer's
  iteration limit or otherwise fail to converge are recorded as `NA`.
* The "may not have found global best fit" check now compares the 5 *best* fits
  (it previously compared the 5 worst) and no longer errors when fewer than 5 starts succeed.
* `steady_state()` for `neg.exp.limit` now returns a number (`Inf` when B > 1e-4, otherwise
  A / k) instead of a string, fixing errors in `summary()` and `bootstrap_parameters()`.
* `bootstrap_parameters()` records failed replicates as `NA` instead of erroring.
* `time_to_prop_mass_remaining()` now uses `uniroot()`: exact, fast, independent of time
  units, and returns `NA` with a warning when the threshold is never reached.
* Plot curves use a fixed number of points regardless of time units; `plot_multiple_fits()`
  no longer clips mass values above 1.
* Fixed Weibull and discrete.series formula display in `plot.litfit()`.
* `plot.litfit()` always shows the (0, 1) starting point on the y axis (#7).
* All six models are now documented in `?fit_litter`; `neg.exp` had been accidentally
  commented out and `neg.exp.limit` was missing (#22).
* `time_to_prop_mass_remaining()` gives a clear error for a missing `threshold.mass`.
* Restored input-validation tests that were lost in 0.1.5, and greatly expanded the test suite
  (edge cases for inputs, every model, derived quantities checked against closed-form results,
  bootstrap failures, methods and plots).

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

* removed plyr dependency for long term ease of maintenance 
* updated testing framework to testthat v3 style
* fixed a testing instability that was causing occasional warnings
* updating doi syntax

# litterfitter 0.1.3

* updated package level documentation
* improved documentation for a number of functions 
* moving github home to traitecoevo

# litterfitter 0.1.2

* Added a `NEWS.md` file to track changes to the package.
