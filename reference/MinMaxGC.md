# Maxima and Minima

Returns the maxima and minima of the input values.

## Usage

``` r
max_gc(..., na.rm = TRUE, allmissing_NA = TRUE)

min_gc(..., na.rm = TRUE, allmissing_NA = TRUE)
```

## Arguments

- ...:

  `numeric` or `character` arguments

- na.rm:

  a `logical` indicating whether missing values should be removed.

- allmissing_NA:

  a `logical` indicating whether `NA` should be returned when there are
  no non-missing arguments passed to
  [min](https://rdrr.io/r/base/Extremes.html) or
  [max](https://rdrr.io/r/base/Extremes.html) (often because
  `na.rm = TRUE` but all values are `NA`)

## Value

If `allmissing_NA = FALSE`, identical to
[min](https://rdrr.io/r/base/Extremes.html) or
[max](https://rdrr.io/r/base/Extremes.html).

If `allmissing_NA = TRUE`, identical to
[min](https://rdrr.io/r/base/Extremes.html) or
[max](https://rdrr.io/r/base/Extremes.html) except that, in cases where
[min](https://rdrr.io/r/base/Extremes.html) or
[max](https://rdrr.io/r/base/Extremes.html) would return an infinite
value and raise a warning because there are no non-missing arguments,
`min_gc` and `max_gc` return `NA`

## Details

These functions are wrappers for
[min](https://rdrr.io/r/base/Extremes.html) and
[max](https://rdrr.io/r/base/Extremes.html), with the additional
argument `allmissing_NA`.
