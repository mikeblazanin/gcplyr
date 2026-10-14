# Predict data by linear interpolation from existing data

Predict data by linear interpolation from existing data

## Usage

``` r
predict_interpolation(
  x,
  y,
  newdata,
  extrapolate_predictions = TRUE,
  na.rm = TRUE
)
```

## Arguments

- x:

  A `vector` of known predictor values.

- y:

  A `vector` of known response values.

- newdata:

  A `vector` of new predictor values for which the response value will
  be predicted

- extrapolate_predictions:

  Boolean indicating whether values of `newdata` that are out of the
  domain of `x` should be predicted (by extrapolating the slope from the
  endpoints of `x`). If `FALSE`, such values will be returned as `NA`.

- na.rm:

  `logical` whether `NA`'s should be removed before making predictions

## Value

A `vector` of response values for each predictor value in `newdata`
