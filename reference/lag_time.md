# Calculate lag time

Lag time is calculated by projecting a tangent line at the point of
maximum (per-capita) derivative backwards to find the time when it
intersects with the minimum `y`-value

## Usage

``` r
lag_time(
  x = NULL,
  y = NULL,
  deriv = NULL,
  blank = NULL,
  trans_y = "log",
  na.rm = TRUE,
  slope = NULL,
  x1 = NULL,
  y1 = NULL,
  y0 = NULL,
  warn_logtransform_warnings = TRUE,
  warn_logtransform_infinite = TRUE,
  warn_min_y_mismatch = TRUE,
  warn_multiple_maxderiv = TRUE,
  warn_one_lag = TRUE,
  warn_no_lag = TRUE,
  warn_blank_length = TRUE
)
```

## Arguments

- x:

  `vector` of `x` values (typically time)

- y:

  `vector` of `y` values (typically density)

- deriv:

  `vector` of derivative values (typically per-capita derivative)

- blank:

  `y`-value associated with a "blank" where the density is 0. Is
  required when `trans_y = TRUE`.

  A `vector` of `blank` values may be specified only when all of
  `slope`, `x1`, `y1`, and `y0` are provided as `vector`s

- trans_y:

  One of `c("linear", "log")` specifying the transformation of
  `y`-values.

  `'log'` is the default, producing calculations of lag time assuming a
  transition to exponential growth

  `'linear'` is available for alternate uses

- na.rm:

  a `logical` indicating whether missing values or values that become
  `NA` or infinite during log-transformation should be removed

- slope:

  Slope to project from `x1`,`y1` to `y0` (typically per-capita growth
  rate). If not provided, will be calculated as `max(deriv)`

- x1:

  `x` value (typically time) to project slope from. If not provided,
  will be calculated as `x[which.max(deriv)]`.

- y1:

  `y` value (typically density) to project slope from. If not provided,
  will be calculated as `y[which.max(deriv)]`.

- y0:

  `y` value (typically density) to find intersection of slope from `x1`,
  `y1` with. If not provided, will be calculated as `min(y)`

- warn_logtransform_warnings:

  `logical` whether warning should be issued when `log(y)` produced
  warnings.

- warn_logtransform_infinite:

  `logical` whether warning should be issued when `log(y)` produced
  infinite values that will be treated as `NA`.

- warn_min_y_mismatch:

  `logical` whether warning should be issued when `min(y)` does not
  equal `min(y[!is.na(x)])`.

- warn_multiple_maxderiv:

  `logical` whether warning should be issued when there are multiple
  points in `deriv` that are tied for the highest, and only the first
  will be used.

- warn_one_lag:

  `logical` whether warning should be issued when some, but not all,
  inputs are vectorized, and only one lag time value will be returned.

- warn_no_lag:

  `logical` whether warning should be issued when calculated lag time is
  less than the minimum value of `x`.

- warn_blank_length:

  `logical` whether warning should be issued when an unexpected number
  of `blank` values was provided and only the first will be used

## Value

Typically a scalar of the lag time in units of `x`. See Details for
cases when value will be a `vector`.

## Details

For most typical uses, simply supply `x`, `y`, and `deriv` (using the
per-capita derivative and `trans_y = 'log'`).

Advanced users may wish to use alternate values for the slope of the
tangent line (`slope`), origination point of the tangent line (`x1`,
`y1`), or minimum `y`-value `y0`. If specified, these values will
override the default calculations. If and only if all of `slope`, `x1`,
`y1`, and `y0` are provided, `lag_time` is vectorized on their inputs
and will return a `vector` of lag time values.
