# Calculate derivatives of vector of data

Provided a `vector` of `y` values, this function returns either the
plain or per-capita difference or derivative between sequential values

## Usage

``` r
calc_deriv(
  y,
  x = NULL,
  return = "derivative",
  percapita = FALSE,
  x_scale = 1,
  blank = NULL,
  subset_by = NULL,
  window_width = NULL,
  window_width_n = NULL,
  window_width_frac = NULL,
  window_width_n_frac = NULL,
  trans_y = "linear",
  na.rm = TRUE,
  warn_ungrouped = TRUE,
  warn_logtransform_warnings = TRUE,
  warn_logtransform_infinite = TRUE,
  warn_window_toosmall = TRUE
)
```

## Arguments

- y:

  Data to calculate difference or derivative of

- x:

  `vector` of `x` values provided as a simple `numeric`.

- return:

  One of `c("difference", "derivative")` for whether the differences in
  `y` should be returned, or the derivative of `y` with respect to `x`

- percapita:

  When `percapita = TRUE`, the per-capita difference or derivative is
  returned

- x_scale:

  `numeric` to scale `x` by in derivative calculation

  Set `x_scale` to the ratio of the units of `x` to the desired units.
  E.g. if `x` is in seconds, but the desired derivative is in units of
  /minute, set `x_scale = 60` (since there are 60 seconds in 1 minute).

- blank:

  `y`-value associated with a "blank" where the density is 0. Is
  required when `percapita = TRUE`.

  If a `vector` of `blank` values is specified, `blank` values are
  assumed to be in the same order as `unique(subset_by)`

- subset_by:

  An optional `vector` as long as `y`. `y` will be split by the unique
  values of this `vector` and the derivative for each group will be
  calculated independently of the others.

  This provides an internally-implemented approach similar to
  [group_by](https://dplyr.tidyverse.org/reference/group_by.html) and
  [mutate](https://dplyr.tidyverse.org/reference/mutate.html)

- window_width, window_width_n, window_width_frac, window_width_n_frac:

  Set how many data points are used to determine the slope at each
  point.

  When all are `NULL`, `calc_deriv` calculates the difference or
  derivative of each point with the next point, appending `NA` at the
  end.

  When one or multiple are specified, a linear regression is fit to all
  points in the window to determine the slope.

  `window_width_n` specifies the width of the window in number of data
  points. `window_width` specifies the width of the window in units of
  `x`. `window_width_n_frac` specifies the width of the window as a
  fraction of the total number of data points.

  When using multiple window specifications at the same time, windows
  are conservative. Points included in each window will meet all of the
  `window_width`, `window_width_n`, and `window_width_n_frac`.

  A value of `window_width_n = 3` or `window_width_n = 5` is often a
  good default.

- trans_y:

  One of `c("linear", "log")` specifying the transformation of
  `y`-values.

  `'log'` is only available when calculating per-capita derivatives
  using a fitting approach (when non-default values are specified for
  `window_width` or `window_width_n`).

  For per-capita growth expected to be exponential or
  nearly-exponential, `"log"` is recommended, since exponential growth
  is linear when log-transformed. However, log-transformations must be
  used with care, since `y`-values at or below 0 will become undefined
  and results will be more sensitive to incorrect values of `blank`.

- na.rm:

  `logical` whether `NA`'s should be removed before analyzing

- warn_ungrouped:

  `logical` whether warning should be issued when `calc_deriv` is being
  called on ungrouped data and `subset_by = NULL`.

- warn_logtransform_warnings:

  `logical` whether warning should be issued when `log(y)` produced
  warnings.

- warn_logtransform_infinite:

  `logical` whether warning should be issued when `log(y)` produced
  infinite values that will be treated as `NA`.

- warn_window_toosmall:

  `logical` whether warning should be issued when only one data point is
  in the window set by `window_width_n`, `window_width`, or
  `window_width_n_frac`, and so `NA` will be returned.

## Value

A `vector` of values for the plain (if `percapita = FALSE`) or
per-capita (if `percapita = TRUE`) difference (if
`return = "difference"`) or derivative (if `return = "derivative"`)
between `y` values. `vector` will be the same length as `y`, with `NA`
values at the ends

## Details

For per-capita derivatives, `trans_y = 'linear'` and `trans_y = 'log'`
approach the same value as time resolution increases.

For instance, let's assume exponential growth \$N = e^rt\$ with
per-capita growth rate \$r\$.

With `trans_y = 'linear'`, note that \$dN/dt = r e^rt = r N\$. So we can
calculate per-capita growth rate as \$r = dN/dt \* 1/N\$.

With `trans_y = 'log'`, note that \$log(N) = log(e^rt) = rt\$. So we can
calculate per-capita growth rate as the slope of a linear fit of
\$log(N)\$ against time, \$r = log(N)/t\$.
