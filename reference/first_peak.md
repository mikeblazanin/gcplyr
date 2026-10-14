# Find the first local maxima of a `numeric` `vector`

This function has been deprecated in favor of the identical new function
[first_maxima](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)

## Usage

``` r
first_peak(
  y,
  x = NULL,
  window_width = NULL,
  window_width_n = NULL,
  window_height = NULL,
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

- window_width:

  Width of the window (in units of `x`) used to search for local
  extrema. A narrower width will be more sensitive to narrow local
  maxima/minima, while a wider width will be less sensitive to local
  maxima/minima.

- window_width_n:

  The maximum number of data points a single extrema-search step is
  allowed to take. For example, when maxima-finding, the function will
  not pass a valley consisting of more than `window_width_n` data
  points.

  A smaller `window_width_n` will be more sensitive to narrow local
  maxima/minima, while a larger `window_width_n` will be less sensitive
  to narrow local maxima/minima.

  If not provided, defaults to `~0.2*length(y)`

- window_height:

  The maximum change in `y` a single extrema-search step is allowed to
  take. For example, when maxima-finding, the function will not pass a
  valley deeper than `window_height`.

  A smaller `window_height` will be more sensitive to shallow local
  maxima/minima, while a larger `window_height` will be less sensitive
  to shallow maxima/minima.

- return:

  One of `c("index", "x", "y")`, determining whether the function will
  return the index, `x` value, or `y` value associated with the first
  maxima in `y` values

- return_endpoints:

  Should the first or last value in `y` be allowed to be returned?

- ...:

  Other parameters to pass to
  [find_local_extrema](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)

## Value

If `return = "index"`, a `vector` of indices corresponding to local
extrema in the data

If `return = "x"`, a `vector` of `x` values corresponding to local
extrema in the data

If `return = "y"`, a `vector` of `y` values corresponding to local
extrema in the data

## Details

This function takes a `vector` of `y` values and returns the index (by
default) of the first local maxima. It serves as a shortcut for
`find_local_extrema(return_maxima = TRUE, return_minima = FALSE)[1]`

If none of `window_width`, `window_width_n`, or `window_height` are
provided, default value of `window_width_n` will be used.

## See also

[first_maxima](https://mikeblazanin.github.io/gcplyr/reference/ExtremaFunctions.md)
