# Find local extrema of a numeric vector

These functions take a `vector` of `y` values and identify local
extrema.

## Usage

``` r
find_local_extrema(
  y,
  x = NULL,
  window_width = NULL,
  window_width_n = NULL,
  window_height = NULL,
  window_width_frac = NULL,
  window_width_n_frac = NULL,
  return = "index",
  return_maxima = TRUE,
  return_minima = TRUE,
  return_endpoints = TRUE,
  subset = NULL,
  na.rm = TRUE,
  width_limit = NULL,
  width_limit_n = NULL,
  height_limit = NULL
)

first_maxima(
  y,
  x = NULL,
  window_width = NULL,
  window_width_n = NULL,
  window_height = NULL,
  window_width_frac = NULL,
  window_width_n_frac = 0.2,
  return = "index",
  return_endpoints = TRUE,
  ...
)

first_minima(
  y,
  x = NULL,
  window_width = NULL,
  window_width_n = NULL,
  window_height = NULL,
  window_width_frac = NULL,
  window_width_n_frac = 0.2,
  return = "index",
  return_endpoints = TRUE,
  ...
)
```

## Arguments

- y:

  `numeric` `vector` of `y` values in which to identify local extrema

- x:

  Optional `numeric` `vector` of corresponding `x` values

- window_width, window_width_n, window_height, window_width_frac,
  window_width_n_frac:

  Arguments that set the width/height of the window used to search for
  local extrema.

  `window_width` is in units of `x`.

  `window_width_n` is in units of number of data points.

  `window_height` is the maximum change in `y` a single extrema-search
  step is allowed to take.

  `window_width_n_frac` is as a fraction of the total number of data
  points.

  For example, the function will not pass a peak or valley more than
  `window_width_n` data points wide, nor a peak/valley taller or deeper
  than `window_height`.

  A narrower width will be more sensitive to narrow local maxima/minima,
  while a wider width will be less sensitive to local maxima/minima. A
  smaller height will be more sensitive to shallow local maxima/minima,
  while a larger height will be less sensitive to shallow maxima/minima.

- return:

  One of `c("index", "x", "y")`, determining whether the function will
  return the index, `x` value, or `y` value associated with the
  identified extremas

- return_maxima, return_minima:

  `logical` for which classes of local extrema to return

- return_endpoints:

  Should the first and last values in `y` be included if they are in the
  returned `vector` of extrema?

- subset:

  A `vector` of `logical` values indicating which `x` and `y` values
  should be included (`TRUE`) or excluded (`FALSE`).

  If `return = "index"`, index will be for the whole `vector` and not
  the subset of the `vector`

- na.rm:

  `logical` whether `NA`'s should be removed before analyzing

- width_limit:

  Deprecated, use `window_width` instead

- width_limit_n:

  Deprecated, use `window_width_n` instead

- height_limit:

  Deprecated, use `window_height` instead

- ...:

  (for `first_maxima` and `first_minima`), other parameters to pass to
  `find_local_extrema`

## Value

`find_local_extrema` returns a `vector` corresponding to all the found
local extrema.

`first_maxima` returns only the first maxima, so is a shortcut for
`find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]`

`first_minima` returns only the first minima, so is a shortcut for
`find_local_extrema(return_maxima = FALSE, return_minima = TRUE)[1]`

If `return = "index"`, the returned value(s) are the indices
corresponding to local extrema in the data

If `return = "x"`, the returned value(s) are the `x` value(s)
corresponding to local extrema in the data

If `return = "y"`, the returned value(s) are the `y` value(s)
corresponding to local extrema in the data

## Details

For `find_local_extrema`, one of `window_width`, `window_width_n`,
`window_height`, or `window_width_n_frac` must be provided.

For `first_minima` or `first_maxima`, set `window_width_n_frac = NULL`
to override default width behavior.

If multiple of `window_width`, `window_width_n`, `window_height`, or
`window_width_n_frac` are provided, steps are limited conservatively (a
single step must meet all criteria).

In the case of exact ties in `y` values within a window, only the first
local extrema is returned.
