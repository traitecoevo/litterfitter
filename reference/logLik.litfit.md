# Log-likelihood of a litfit object

Returns the log-likelihood of the fitted model, which allows the
standard [`AIC`](https://rdrr.io/r/stats/AIC.html) and
[`BIC`](https://rdrr.io/r/stats/AIC.html) functions to be used to
compare several `litfit` objects, e.g. `AIC(fit1, fit2)`.

## Usage

``` r
# S3 method for class 'litfit'
logLik(object, ...)
```

## Arguments

- object:

  a `litfit` object

- ...:

  unused

## Value

an object of class `logLik`

## Details

The degrees of freedom are the number of fitted curve parameters (the
residual standard deviation is not counted), consistent with the
`fitAIC`, `fitAICc` and `fitBIC` values stored in the object and with
Cornwell & Weedon (2013).
