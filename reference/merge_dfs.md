# Collapse a list of data.frame's, or merge two data.frame's together

This function is essentially a wrapper for any of `dplyr`'s
[mutate-joins](https://dplyr.tidyverse.org/reference/mutate-joins.html)
(by default, a
[full_join](https://dplyr.tidyverse.org/reference/mutate-joins.html)).
The most typical use of this function is to merge designs with measures
data, or to use the `collapse` functionality to merge a `list` of
`data.frame`s into a single `data.frame`. Merging is done by column
names that match between `x` and `y`.

## Usage

``` r
merge_dfs(
  x,
  y = NULL,
  by = NULL,
  drop = FALSE,
  collapse = FALSE,
  names_to = NA,
  join = "full",
  warn_morerows = TRUE,
  ...
)
```

## Arguments

- x:

  First `data.frame`, or `list` of `data.frame`s, to be joined

- y:

  Second `data.frame`, or `list` of `data.frame`s, to be joined

- by:

  A `character` `vector` of variables to join by, passed directly to the
  join function

- drop:

  Should only `complete_cases` of the resulting `data.frame` be
  returned?

- collapse:

  A `logical` indicating whether `x` or `y` is a `list` containing
  `data.frame`s that should be merged together before being merged with
  the other

- names_to:

  Column name for where `names(x)` or `names(y)` will be entered in if
  `collapse = TRUE`.

  If a value of `NA` then `names(x)` or `names(y)` will not be put into
  a column in the returned `data.frame`

- join:

  Type of join used to merge `x` and `y`. Options are 'full' (default),
  'inner', 'left', and 'right'.

  - A `full` join keeps all observations in `x` and `y`

  - A `left` join keeps all observations in `x`

  - A `right` join keeps all observations in `y`

  - An `inner` join only keeps observations found in both `x` and `y`
    (inner joins are not appropriate in most cases because observations
    are frequently dropped).

  See
  [full_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [left_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [right_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  or
  [inner_join](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for more details

- warn_morerows:

  `logical`, should a warning be passed when the output has more rows
  than `x` and more rows than `y`?

- ...:

  Other arguments to pass to the underlying join function. See
  [full_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [left_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  [right_join](https://dplyr.tidyverse.org/reference/mutate-joins.html),
  or
  [inner_join](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for options.

## Value

`data.frame` containing merged output of `x` and `y`
