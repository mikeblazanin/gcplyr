# Separate a column into multiple columns

This function is primarily a wrapper for
[separate](https://tidyr.tidyverse.org/reference/separate.html), which
turns a single `character` column into multiple columns

## Usage

``` r
separate_tidy(
  data,
  col,
  into = NULL,
  sep = "_",
  coerce_NA = TRUE,
  na.strings = "NA",
  message_inferred_into = TRUE,
  ...
)
```

## Arguments

- data:

  A `data.frame`

- col:

  Column name or position

- into:

  A `character` `vector` of the new column names. Use `NA` to omit the
  variable in the output.

  If `NULL`, `separate_tidy` will attempt to infer the new column names
  by splitting the column name of `col`

- sep:

  Separator between columns passed to
  [separate](https://tidyr.tidyverse.org/reference/separate.html):

  If `character`, `sep` is interpreted as a regular expression.

  If `numeric`, `sep` is interpreted as character positions to split at.
  Positive values start at 1 at the far-left of the `string`; negative
  values start at -1 at the far-right of the `string`. The length of
  `sep` should be one less than `into`

- coerce_NA:

  `logical` dictating if `string`s matching any of `na.strings` will be
  coerced into `NA` values after separating.

- na.strings:

  A `character` `vector` of `string`s which are to be interpreted as
  `NA` values if `coerce_NA == TRUE`

- message_inferred_into:

  `logical` whether column names for `into` should be printed in a
  message when inferred

- ...:

  Other arguments passed to
  [separate](https://tidyr.tidyverse.org/reference/separate.html)

## Value

A `data.frame` containing new columns in the place of `col`
