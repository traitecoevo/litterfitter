# Package index

## Fitting mass loss

Fit decomposition models to single-cohort mass loss data.

- [`fit_litter()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
  : Fit different models to single cohort decomposition data
- [`fit_litter_all()`](https://traitecoevo.github.io/litterfitter/reference/fit_litter_all.md)
  : Fit several decomposition models to the same data
- [`as_litfit_list()`](https://traitecoevo.github.io/litterfitter/reference/as_litfit_list.md)
  : Combine existing fits into a litfit_list
- [`print(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  [`glance(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  [`tidy(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  [`augment(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  [`coef(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  [`` `[`( ``*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_list_methods.md)
  : Methods for litfit_list objects
- [`predict(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/predict.litfit.md)
  : Predict method for litfit objects
- [`logLik(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/logLik.litfit.md)
  : Log-likelihood of a litfit object

## Element dynamics

Fit element (nutrient) remaining as a function of mass remaining.

- [`fit_element()`](https://traitecoevo.github.io/litterfitter/reference/fit_element.md)
  : Fit element (nutrient) dynamics during decomposition
- [`print(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`coef(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`fitted(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`residuals(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`nobs(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`logLik(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`predict(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`tidy(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`glance(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  [`augment(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/elementfit_methods.md)
  : Methods for elementfit objects

## Derived quantities

- [`time_to_prop_mass_remaining()`](https://traitecoevo.github.io/litterfitter/reference/time_to_prop_mass_remaining.md)
  : Get the predicted time until half mass loss for a litter
  decomposition trajectory
- [`steady_state()`](https://traitecoevo.github.io/litterfitter/reference/steady_state.md)
  : Estimate Steady State Biomass

## Uncertainty

- [`bootstrap_parameters()`](https://traitecoevo.github.io/litterfitter/reference/bootstrap_parameters.md)
  : Create a bootstrap distribution of a particular coefficient from a
  model fit
- [`summary(`*`<litfit_bootstrap>`*`)`](https://traitecoevo.github.io/litterfitter/reference/summary.litfit_bootstrap.md)
  : Summarise a bootstrap distribution of parameters from a litfit
  object

## Plotting

Base graphics methods and ggplot2
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods.

- [`plot(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit.md)
  : Plot a decomposition trajectory and its fitted curve
- [`plot(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/plot.elementfit.md)
  : Plot an element fit
- [`plot(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit_list.md)
  : Plot several fits to the same data
- [`plot_multiple_fits()`](https://traitecoevo.github.io/litterfitter/reference/plot_multiple_fits.md)
  : Fit and plot several models on one graph with model selection
  results
- [`plot(`*`<litfit_bootstrap>`*`)`](https://traitecoevo.github.io/litterfitter/reference/plot.litfit_bootstrap.md)
  : Plot bootstrap distributions
- [`autoplot(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litterfitter_autoplot.md)
  [`autoplot(`*`<elementfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litterfitter_autoplot.md)
  [`autoplot(`*`<litfit_list>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litterfitter_autoplot.md)
  [`autoplot(`*`<litfit_bootstrap>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litterfitter_autoplot.md)
  : ggplot2 plots of litterfitter fits

## Tidy output

Results as data frames, following the conventions of the broom package.

- [`tidy(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_tidiers.md)
  [`glance(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_tidiers.md)
  [`augment(`*`<litfit>`*`)`](https://traitecoevo.github.io/litterfitter/reference/litfit_tidiers.md)
  : Tidy, glance and augment methods for litfit objects
- [`tidy(`*`<litfit_bootstrap>`*`)`](https://traitecoevo.github.io/litterfitter/reference/tidy.litfit_bootstrap.md)
  : Tidy a bootstrap distribution
- [`reexports`](https://traitecoevo.github.io/litterfitter/reference/reexports.md)
  [`tidy`](https://traitecoevo.github.io/litterfitter/reference/reexports.md)
  [`glance`](https://traitecoevo.github.io/litterfitter/reference/reexports.md)
  [`augment`](https://traitecoevo.github.io/litterfitter/reference/reexports.md)
  : Objects exported from other packages

## Data and package

- [`pineneedles`](https://traitecoevo.github.io/litterfitter/reference/pineneedles.md)
  : decomposition trajectory for pine needles
- [`litterfitter-package`](https://traitecoevo.github.io/litterfitter/reference/litterfitter.md)
  [`litterfitter`](https://traitecoevo.github.io/litterfitter/reference/litterfitter.md)
  : litterfitter: methods for fitting curves to litter decomposition
  trajectories
