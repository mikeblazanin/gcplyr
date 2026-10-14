# Turn tidydesign into block format

This function allows users to convert designs created with tidydesign
into a block format for easy output to csv for inclusion in lab
notebooks, etc in a human-readable format

## Usage

``` r
block_tidydesign(
  tidydesign,
  collapse = NULL,
  wellnames_sep = "_",
  wellnames_colname = "Well"
)
```

## Arguments

- tidydesign:

  A tidydesign `data.frame` (e.g. as created by
  [make_tidydesign](https://mikeblazanin.github.io/gcplyr/reference/make_tidydesign.md))

- collapse:

  `NULL` or a `string` to use for concatenating design elements
  together. If `NULL` each design column will be put into its own block.
  If a `string`, that `string` will be used to
  [paste](https://rdrr.io/r/base/paste.html) together all design
  elements and all design elements will be returned in a single block

- wellnames_sep:

  A `string` used when concatenating rownames and column names to create
  well names

- wellnames_colname:

  Header for newly-created column containing the well names

## Value

A `list` of blockdesign `data.frame`s (if `collapse` is not `NULL` the
`list` is of length 1
