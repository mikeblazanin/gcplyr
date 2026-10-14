# Where is the Min() or Max() or first TRUE or FALSE?

Determines the location, i.e. index, of the (first) minimum or maximum
of a `numeric` (or `logical`) `vector`.

## Usage

``` r
which_min_gc(x, empty_NA = TRUE)

which_max_gc(x, empty_NA = TRUE)
```

## Arguments

- x:

  `numeric` (`logical`, `integer`, or `double`) `vector` or an R object
  for which the internal coercion to `double` works whose min or max is
  searched for.

- empty_NA:

  `logical`, indicating if an empty value should be returned as `NA`
  (the default) or as `integer(0)` (the same as
  [which.min](https://rdrr.io/r/base/which.min.html) and
  [which.max](https://rdrr.io/r/base/which.min.html)).

## Value

If `empty_NA = FALSE`, identical to
[which.min](https://rdrr.io/r/base/which.min.html) or
[which.max](https://rdrr.io/r/base/which.min.html)

If `empty_NA = TRUE`, identical to
[which.min](https://rdrr.io/r/base/which.min.html) or
[which.max](https://rdrr.io/r/base/which.min.html) except that, in cases
where [which.min](https://rdrr.io/r/base/which.min.html) or
[which.max](https://rdrr.io/r/base/which.min.html) would return
`integer(0)`, `which_min_gc` and `which_max_gc` return `NA`

## Details

These functions are wrappers for
[which.min](https://rdrr.io/r/base/which.min.html) and
[which.max](https://rdrr.io/r/base/which.min.html), with the additional
argument `empty_NA`.
