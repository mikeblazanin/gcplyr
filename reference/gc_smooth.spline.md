# Fit a Smoothing Spline

This function is a wrapper for
[smooth.spline](https://rdrr.io/r/stats/smooth.spline.html), which fits
a cubic smoothing spline to the supplied data, but includes the option
to remove `NA` values, and returns values in the original order.

## Usage

``` r
gc_smooth.spline(x, y = NULL, ..., na.rm = TRUE)
```

## Arguments

- x:

  A `vector` giving the values of the predictor variable.

- y:

  A `vector` giving the values of the response variable. If `y` is
  missing or `NULL`, the responses are assumed to be specified by `x`,
  with `x` the index `vector`.

- ...:

  Additional arguments passed to
  [smooth.spline](https://rdrr.io/r/stats/smooth.spline.html).

- na.rm:

  `logical` whether `NA`'s should be removed before analyzing. Required
  to be `TRUE` if any `x` or `y` values are `NA`.

## Value

Similar to [smooth.spline](https://rdrr.io/r/stats/smooth.spline.html),
an object of class "`smooth.spline`" with many components. Differs in
that `x`, `y`, and `w` have `NA`'s at any indices where `x` or `y` were
`NA` in the inputs, and `x`, `y`, and `w` are returned to match the
input `x` in order and length

## Details

See [smooth.spline](https://rdrr.io/r/stats/smooth.spline.html)
