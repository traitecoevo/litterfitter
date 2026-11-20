## Submission

This is an update from 0.1.4 (the current CRAN version) to 0.3.0. Versions
0.1.5 and 0.2.0 were not released to CRAN; their changes are included here.
In this version I have:

- Fixed the README DOI link that gave a NOTE on CRAN's r-devel Debian checks
  ("possibly invalid file URI").
- Fixed several bugs (default model selection, convergence check, steady
  state for the `neg.exp.limit` model, bootstrap failures, time-to-threshold
  calculation).
- Added `tidy()`, `glance()` and `augment()` methods, `fit_litter_all()` for
  model comparison, and `fit_element()` for element (nutrient) dynamics.
- Added `autoplot()` methods and modernised the base plots.
- Used analytic gradients for model fitting.
- New dependencies: `generics` and `utils` (Imports); `ggplot2` and `vdiffr`
  (Suggests, used conditionally).

## Test environments

- local macOS (aarch64), R 4.6.1
- win-builder: R-devel
- macOS builder: R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are no reverse dependencies on CRAN.
