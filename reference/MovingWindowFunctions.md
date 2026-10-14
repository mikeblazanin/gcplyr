# Moving window smoothing

These functions use a moving window to smooth data

## Usage

``` r
moving_average(
  formula = NULL,
  data = NULL,
  x = NULL,
  y = NULL,
  window_width_n = NULL,
  window_width = NULL,
  window_width_n_frac = NULL,
  window_width_frac = NULL,
  na.rm = TRUE,
  warn_nonnumeric_sort = TRUE
)

moving_median(
  formula = NULL,
  data = NULL,
  x = NULL,
  y = NULL,
  window_width_n = NULL,
  window_width = NULL,
  window_width_n_frac = NULL,
  window_width_frac = NULL,
  na.rm = TRUE,
  warn_nonnumeric_sort = TRUE
)
```

## Arguments

- formula:

  Formula specifying the `numeric` response (density) and `numeric`
  predictor (time).

- data:

  `data.frame` containing variables in `formula`

- x:

  A `vector` of predictor values to smooth along (e.g. time)

- y:

  A `vector` of response values to be smoothed (e.g. density).

- window_width_n:

  Number of data points wide the moving window is (therefore, must be an
  odd number of points)

- window_width:

  Width of the moving window (in units of `x`)

- window_width_n_frac:

  Width of the window (as a fraction of the total number of data
  points).

- window_width_frac:

  Width of the window (as a fraction of the range of `x`)

- na.rm:

  `logical` whether `NA`'s should be removed before analyzing

- warn_nonnumeric_sort:

  `logical` whether warning should be issued when predictor variable
  that data is sorted by is non-`numeric`.

## Value

`vector` of smoothed data, with `NA`'s appended at both ends

## Details

Either `x` and `y` or `formula` and `data` must be provided.

Values of `NULL` or `NA` will be ignored for any of `window_width_n`,
`window_width`, `window_width_n_frac`, or `window_width_frac`
