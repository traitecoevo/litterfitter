# Combine existing fits into a litfit_list

Combine existing fits into a litfit_list

## Usage

``` r
as_litfit_list(...)
```

## Arguments

- ...:

  `litfit` objects, or a single list of them. Names are used as fit
  names; unnamed fits are named by their model (made unique if needed).

## Value

A `litfit_list`.

## Details

Information criteria are only comparable between fits to the same data,
so a warning is given if the fits were made to different data.

## Examples

``` r
f1 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "neg.exp",
  iters = 50, quiet = TRUE
)
f2 <- fit_litter(pineneedles$Year, pineneedles$Mass.remaining, "weibull",
  iters = 50, quiet = TRUE
)
fits <- as_litfit_list(f1, f2)
glance(fits)
#>       fit   model nobs nparams      sigma   logLik       AIC      AICc
#> 1 neg.exp neg.exp    6       1 0.05224361 9.197395 -16.39479 -15.39479
#> 2 weibull weibull    6       2 0.05001515 9.458945 -14.91789 -10.91789
#>         BIC converged time_to_50 steady_state delta_AICc
#> 1 -16.60303      TRUE   2.493138     3.596837     0.0000
#> 2 -15.33437      TRUE   2.517099     3.405444     4.4769
```
