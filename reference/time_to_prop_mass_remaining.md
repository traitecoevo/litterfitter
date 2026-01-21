# Get the predicted time until half mass loss for a litter decomposition trajectory

Get estimated time to 0.5 (or an alternate threshold) mass loss from a
particular fit to a litter decomposition trajectory

## Usage

``` r
time_to_prop_mass_remaining(x,threshold.mass=0.5)
```

## Arguments

- x:

  a litfit object

- threshold.mass:

  mass loss threshold in proportion, default is 0.5

## Value

numeric value that represents time to a specified mass loss percentage

## Details

this function finds the time to a specified mass loss percentage

## See also

[`fit_litter`](http://traitecoevo.github.io/litterfitter/reference/fit_litter.md)
[`plot.litfit`](http://traitecoevo.github.io/litterfitter/reference/plot.litfit.md)

## Author

Will Cornwell

## Examples

``` r
fit <- fit_litter(
  time = pineneedles$Year, mass.remaining = pineneedles$Mass.remaining,
  model = "neg.exp", iters = 1000
)
#> Number of successful fits:  974  out of 1000 
time_to_prop_mass_remaining(fit, threshold.mass = 0.5)
#> [1] 2.4932
```
