# Smooth data

This function calls other functions to smooth growth curve data

## Usage

``` r
smooth_data(
  ...,
  x = NULL,
  y = NULL,
  sm_method,
  subset_by = NULL,
  return_fitobject = FALSE,
  warn_ungrouped = TRUE,
  warn_gam_no_s = TRUE
)
```

## Arguments

- ...:

  Arguments passed to [loess](https://rdrr.io/r/stats/loess.html),
  [gam](https://rdrr.io/pkg/mgcv/man/gam.html),
  [moving_average](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md),
  [moving_median](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md),
  or [smooth.spline](https://rdrr.io/r/stats/smooth.spline.html).
  Typically includes tuning parameter(s), which in some cases are
  required. See Details for more information.

- x:

  An (often optional) `vector` of predictor values to smooth along (e.g.
  time)

- y:

  A `vector` of response values to be smoothed (e.g. density). If
  `NULL`, `formula` and `data` *must* be provided via `...`

- sm_method:

  Argument specifying which smoothing method should be used to smooth
  data. Options include "moving-average", "moving-median", "loess",
  "gam", and "smooth.spline".

- subset_by:

  An optional `vector` as long as `y`. `y` will be split by the unique
  values of this `vector` and the smoothed data for each group will be
  calculated independently of the others.

  This provides an internally-implemented approach similar to
  [group_by](https://dplyr.tidyverse.org/reference/group_by.html) and
  [mutate](https://dplyr.tidyverse.org/reference/mutate.html)

- return_fitobject:

  `logical` whether entire object returned by fitting function should be
  returned. If `FALSE`, just fitted values are returned.

- warn_ungrouped:

  `logical` whether warning should be issued when `smooth_data` is being
  called on ungrouped data and `subset_by = NULL`.

- warn_gam_no_s:

  `logical` whether warning should be issued when
  [gam](https://rdrr.io/pkg/mgcv/man/gam.html) is used without
  [s](https://rdrr.io/pkg/mgcv/man/s.html) in the formula.

## Value

If `return_fitobject == FALSE:`

A `vector`, the same length as `y`, with the now-smoothed `y` values

If `return_fitobject == TRUE:`

A `list` the same length as `unique(subset_by)` where each element is an
object of the same class as returned by the smoothing method (typically
a named `list`-like object)

## Details

For
[moving_average](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md)
and
[moving_median](https://mikeblazanin.github.io/gcplyr/reference/MovingWindowFunctions.md),
passing `window_width` or `window_width_n` via `...` is required.
`window_width` sets the width of the moving window in units of `x`,
while `window_width_n` sets the width in units of number of data points.
Larger values for either will produce more "smoothed" data.

For [loess](https://rdrr.io/r/stats/loess.html), the `span` argument
sets the fraction of data points that should be included in each
calculation. It's typically best to specify, since the default of 0.75
is often too large for growth curves data. Larger values of `span` will
produce more "smoothed" data

For [gam](https://rdrr.io/pkg/mgcv/man/gam.html), both arguments to
[gam](https://rdrr.io/pkg/mgcv/man/gam.html) and
[s](https://rdrr.io/pkg/mgcv/man/s.html) can be provided via `...`. Most
frequently, the `k` argument to [s](https://rdrr.io/pkg/mgcv/man/s.html)
sets the number of "knots" the spline-fitting can use. Smaller values of
`k` will be more "smoothed".

When using `sm_method = "gam"`, advanced users may also modify other
parameters of [s](https://rdrr.io/pkg/mgcv/man/s.html), including the
smoothing basis `bs`. These bases can be thin plate (`bs = "tp"`, the
default), cubic regressions (`bs = "cr"`), or many other options (see
[s](https://rdrr.io/pkg/mgcv/man/s.html)). I recommend leaving the
default thin plate regressions, whose main drawback is that they are
computationally intensive to calculate. For growth curves data, this is
unlikely to be relevant.

As an alternative to passing `y`, for more advanced needs with
[loess](https://rdrr.io/r/stats/loess.html) or
[gam](https://rdrr.io/pkg/mgcv/man/gam.html), `formula` and `data` can
be passed to `smooth_data` via the `...` argument (in lieu of `y`).

In this case, the formula should specify the response (e.g. density) and
predictors. For [gam](https://rdrr.io/pkg/mgcv/man/gam.html) smoothing,
the formula should typically be of the format: `y ~ s(x)`, which uses
[s](https://rdrr.io/pkg/mgcv/man/s.html) to smooth the data. The data
argument should be a `data.frame` containing the variables in the
formula. In such cases, `subset_by` can still be specified as a `vector`
with length `nrow(data)`
