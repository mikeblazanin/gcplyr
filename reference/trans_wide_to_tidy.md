# Pivot wide-shaped into tidy

Essentially a wrapper for
[pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html)
that works on both a single wide-shaped `data.frame` as well as a `list`
of wide-shaped `data.frame`'s

## Usage

``` r
trans_wide_to_tidy(
  wides,
  data_cols = NA,
  id_cols = NA,
  names_to = "Well",
  values_to = "Measurements",
  values_to_numeric = TRUE,
  ...
)
```

## Arguments

- wides:

  A single wide-shaped `data.frame`, or a `list` of wide-shaped
  `data.frame`'s

- data_cols, id_cols:

  Specifies which columns have data vs are ID's (in
  [pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  parlance). Each can be a single `vector` (which will be applied for
  all `data.frame`s) or a `list` of `vector`s, with each `vector`
  corresponding to the same-index `data.frame` in `wides`

  Entries that are `NA` in the `list` will not be used

  If neither `data_cols` nor `id_cols` are specified, user must provide
  arguments to
  [pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  via `...` for at least the `cols` argument and these arguments
  provided via `...` will be used for all `wides` `data.frame`'s

- names_to, values_to:

  Specifies the output column names created by
  [pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html).
  Each can be provided as `vector`s the same length as `wides`

- values_to_numeric:

  `logical` indicating whether values will be coerced to `numeric`. See
  below for when this may be overridden by arguments passed in `...`

- ...:

  Other arguments to be passed to
  [pivot_longer](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  Note that including `values_transform` here will override the behavior
  of `values_to_numeric`

## Value

Pivoted longer `data.frame` (if `wides` is a single `data.frame`) or
`list` of pivoted longer `data.frame`s (if `wides` is a `list` of
`data.frame`s)
