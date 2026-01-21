# Predict method for litfit objects

Generated predicted values for (new) time points from a litfit model fit

## Usage

``` r
# S3 method for class 'litfit'
predict(object,newdata=NULL,...)
```

## Arguments

- object:

  litfit object

- newdata:

  optional vector of new Time points at which to predict mass remaining.
  If not specified, Time points from the original fit are used.

- ...:

  further arguments passed to or from other methods.

## Value

predicted values from a litfit object

## Details

to do

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)

## Author

Will Cornwell

James Weedon

## Examples

``` r
fit <- fit_litter(
  time = c(0, 1, 2, 3, 4, 5, 6), mass.remaining = c(1, 0.9, 1.01, 0.4, 0.6, 0.2, 0.01),
  "neg.exp", iters = 250
)
#> Number of successful fits:  247  out of 250 
predict(fit, newdata = 1:10)
#>  [1] 0.8048061 0.6477129 0.5212834 0.4195320 0.3376420 0.2717363 0.2186951
#>  [8] 0.1760071 0.1416516 0.1140021
```
