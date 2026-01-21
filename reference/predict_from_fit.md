# Predict Using a Specific Model Function

This function computes predictions by applying the specified model
function (`model_fn`) with provided parameters (`params`) at a target
time (`target_time`). It is an internal utility function and is NOT a
generic or S3 method.

## Usage

``` r
predict_from_fit(model_fn, params, target_time)
```

## Arguments

- model_fn:

  A function representing the model used for prediction.

- params:

  A list of parameters to be passed to the model function.

- target_time:

  A numeric value representing the target time for the prediction.

## Value

Numeric. The predicted value computed by the model function.
