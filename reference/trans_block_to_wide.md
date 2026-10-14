# Transform blocks to wides

Takes blocks and returns them in a wide format

## Usage

``` r
trans_block_to_wide(
  blocks,
  wellnames_sep = "",
  nested_metadata = NULL,
  colnames_first = FALSE
)
```

## Arguments

- blocks:

  Blocks, either a single `data.frame` or a `list` of `data.frame`s

- wellnames_sep:

  `string` to use as separator for well names between row name and
  column name (ordered according to `colnames_first`

- nested_metadata:

  A `logical` indicating the existence of nested metadata in the
  blockmeasures `list`, e.g. as is typically output by
  [read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md).
  If `NULL`, will attempt to infer existence of nested metadata

- colnames_first:

  In the wellnames created by
  [paste](https://rdrr.io/r/base/paste.html)-ing the rownames and column
  names, should the column names come first?

## Value

A single wide-shaped `data.frame`
