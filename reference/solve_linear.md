# Return missing information about a line

Takes a set of inputs that is sufficient information to infer a line and
then returns information not provided (either the slope, an `x` point on
the line, or a `y` point on the line)

## Usage

``` r
solve_linear(
  x1,
  y1,
  x2 = NULL,
  y2 = NULL,
  x3 = NULL,
  y3 = NULL,
  m = NULL,
  named = TRUE
)
```

## Arguments

- x1, y1:

  A point on the line

- x2, y2:

  An additional point on the line

- x3, y3:

  An additional point on the line

- m:

  The slope of the line

- named:

  `logical` indicating whether the returned value(s) should be named
  according to what they are (`m`, `x2`, `y2`, `x3`, or `y3`)

## Value

A named `vector` with the missing information from the line:

If `m` and `x2` are provided, `y2` will be returned

If `m` and `y2` are provided, `x2` will be returned

If `x2` and `y2` are provided, but neither `x3` nor `y3` are provided,
`m` will be returned

If `x2` and `y2` are provided and one of `x3` or `y3` are provided, the
other (`y3` or `x3`) will be returned

## Details

Note that there is no requirement that `x1` \< `x2` \< `x3`: the points
can be in any order along the line.

`solve_linear` works with `vector`s of all inputs to solve multiple
lines at once, where the `i`th element of each argument corresponds to
the `i`th output. Note that all lines must be missing the same
information. Input `vector`s will be recycled as necessary.
