# Create method argument for train of growth curve smoothers

This function generates a `list` which is compatible to be used as the
`method` argument to [train](https://rdrr.io/pkg/caret/man/train.html).
This enables users to call
[train](https://rdrr.io/pkg/caret/man/train.html) directly themselves
with
[smooth_data](https://mikeblazanin.github.io/gcplyr/reference/smooth_data.md)
smoothing functions.

## Usage

``` r
makemethod_train_smooth_data(sm_method, tuneGrid = NULL)
```

## Arguments

- sm_method:

  Argument specifying which smoothing method should be used. Options
  include "moving-average", "moving-median", "loess", "gam", and
  "smooth.spline".

- tuneGrid:

  A `data.frame` with possible tuning values. The columns should be
  named the same as the tuning parameters.

  Note that, when using
  [train](https://rdrr.io/pkg/caret/man/train.html), the `tuneGrid` must
  be passed both to this function as well as directly to
  [train](https://rdrr.io/pkg/caret/man/train.html).

## Value

A `list` that can be used as the `method` argument to
[train](https://rdrr.io/pkg/caret/man/train.html). Contains elements:
`library`, `type`, `prob`, `fit`, `parameters`, `grid`, `fit`, and
`predict`.

See documentation on using a custom model model in
[train](https://rdrr.io/pkg/caret/man/train.html) for more details.
