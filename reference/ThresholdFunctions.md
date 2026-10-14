# Find point(s) when a numeric vector crosses some threshold

These functions take a `vector` of `y` values and identify points where
the `y` values cross some `threshold` `y` value.

## Usage

``` r
find_threshold_crosses(
  y,
  x = NULL,
  threshold,
  return = "index",
  return_rising = TRUE,
  return_falling = TRUE,
  return_endpoints = TRUE,
  subset = NULL,
  na.rm = TRUE
)

first_below(
  y,
  x = NULL,
  threshold,
  return = "index",
  return_endpoints = TRUE,
  ...
)

first_above(
  y,
  x = NULL,
  threshold,
  return = "index",
  return_endpoints = TRUE,
  ...
)
```

## Arguments

- y:

  `numeric` `vector` of `y` values in which to identify threshold
  crossing event(s)

- x:

  Optional `numeric` `vector` of corresponding `x` values

- threshold:

  Threshold `y` value of interest

- return:

  One of `c("index", "x")`, determining whether the function will return
  the `index` or `x` value associated with the threshold-crossing event.

  If `index`, it will refer to the data point immediately after the
  crossing event.

  If `x`, it will use linear interpolation and the data points
  immediately before and after the threshold-crossing to return the
  exact `x` value when the threshold crossing occurred

- return_rising:

  `logical` for whether crossing events where `y` rises above
  `threshold` should be returned

- return_falling:

  `logical` for whether crossing events where `y` falls below
  `threshold` should be returned

- return_endpoints:

  `logical` for whether startpoint should be returned when the
  startpoint is above `threshold` and `return_rising = TRUE`, or when
  the startpoint is below `threshold` and `return_falling = TRUE`

- subset:

  A `vector` of `logical` values indicating which `x` and `y` values
  should be included (`TRUE`) or excluded (`FALSE`).

  If `return = "index"`, index will be for the whole `vector` and not
  the subset of the `vector`

- na.rm:

  `logical` whether `NA`'s should be removed before analyzing. If
  `return = 'index'`, indices will refer to the original `y` `vector`
  *including* `NA` values

- ...:

  (for `first_above` and `first_below`) other arguments to pass to
  `find_threshold_crosses`

## Value

`find_threshold_crosses` returns a `vector` corresponding to all the
threshold crossings.

`first_above` returns only the first time the `y` values rise above the
`threshold`, so is a shortcut for
`find_threshold_crosses(return_rising = TRUE, return_falling = FALSE)[1]`

`first_below` returns only the first time the `y` values fall below the
`threshold`, so is a shortcut for
`find_threshold_crosses(return_rising = FALSE, return_falling = TRUE)[1]`

If `return = "index"`, the returned value(s) are the indices immediately
following threshold crossing(s)

If `return = "x"`, the returned value(s) are the `x` value(s)
corresponding to threshold crossing(s)

If no threshold-crossings are detected that meet the criteria, will
return `NA`
