# Calculate area under the curve

This function takes a `vector` of `x` and `y` values and returns a
scalar for the area under the curve, calculated using the trapezoid rule

## Usage

``` r
auc(
  x,
  y,
  xlim = NULL,
  blank = 0,
  subset = NULL,
  na.rm = TRUE,
  neg.rm = FALSE,
  warn_xlim_out_of_range = TRUE,
  warn_negative_y = TRUE
)
```

## Arguments

- x:

  `numeric` `vector` of `x` values

- y:

  `numeric` `vector` of `y` values

- xlim:

  `vector`, of length 2, delimiting the `x` range over which the area
  under the curve should be calculated (where `NA` can be provided for
  the area to be calculated from the start or to the end of the data)

- blank:

  Value to be subtracted from `y` values before calculating area under
  the curve

- subset:

  A `vector` of `logical` values indicating which `x` and `y` values
  should be included (`TRUE`) or excluded (`FALSE`).

- na.rm:

  a `logical` indicating whether missing values should be removed

- neg.rm:

  a `logical` indicating whether `y` values below zero should be treated
  as zeros. If `FALSE`, area under the curve for negative `y` values
  will be calculated normally, effectively subtracting from the returned
  value.

- warn_xlim_out_of_range:

  `logical` whether warning should be issued when `xlim` is lower than
  the lowest `x` value or higher than the highest `x` value.

- warn_negative_y:

  `logical` whether warning should be issued when `neg.rm == FALSE` but
  some `y` values are below 0.

## Value

A scalar for the total area under the curve
