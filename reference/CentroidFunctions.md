# Calculate centroid

This function takes a `vector` of `x` and `y` values and returns the `x`
and/or `y` position of the centroid of mass of the area under the curve

## Usage

``` r
centroid(
  x,
  y,
  return,
  xlim = NULL,
  blank = 0,
  subset = NULL,
  na.rm = TRUE,
  neg.rm = FALSE,
  warn_xlim_out_of_range = TRUE,
  warn_negative_y = TRUE
)

centroid_x(x, y, return = "x", ...)

centroid_y(x, y, return = "y", ...)

centroid_both(x, y, return = "both", ...)
```

## Arguments

- x:

  `numeric` `vector` of `x` values

- y:

  `numeric` `vector` of `y` values

- return:

  One of `c("x", "y", "both")`, determining whether the function will
  return the `x` value of the centroid, the `y` value of the centroid,
  or a `vector` containing `x` then `y`

- xlim:

  `vector`, of length 2, delimiting the `x` range over which the
  centroid should be calculated (where `NA` can be provided for the area
  to be calculated from the start or to the end of the data)

- blank:

  Value to be subtracted from `y` values before calculating the centroid

- subset:

  A `vector` of `logical` values indicating which `x` and `y` values
  should be included (`TRUE`) or excluded (`FALSE`).

- na.rm:

  a `logical` indicating whether missing values should be removed

- neg.rm:

  a `logical` indicating whether `y` values below zero should be treated
  as zeros. If `FALSE`, the centroid for negative `y` values will be
  calculated normally, effectively pulling the centroid towards the `x`
  axis.

- warn_xlim_out_of_range:

  `logical` whether warning should be issued when `xlim` is lower than
  the lowest `x` value or higher than the highest `x` value.

- warn_negative_y:

  `logical` whether warning should be issued when `neg.rm == FALSE` but
  some `y` values are below 0.

- ...:

  Other arguments to pass to `centroid`

## Value

A scalar for the `x` value (if `return = 'x'`) or `y` value (if
`return = 'y'`) of the centroid of the data

## Details

This function uses
[st_centroid](https://r-spatial.github.io/sf/reference/geos_unary.html)
to calculate the centroid of mass
