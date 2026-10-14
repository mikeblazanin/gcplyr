# Paste a list of blocks into a single block

This function uses [paste](https://rdrr.io/r/base/paste.html) to
concatenate the same-location entries of a `list` of `data.frame`s
together (i.e. all the first row-first column values are pasted
together, all the second row-first column values are pasted together,
etc.)

## Usage

``` r
paste_blocks(blocks, sep = "_", nested_metadata = NULL)
```

## Arguments

- blocks:

  Blocks, either a single `data.frame` or a `list` of `data.frame`s

- sep:

  `string` to use as separator for output pasted values

- nested_metadata:

  A `logical` indicating the existence of nested metadata in the
  blockmeasures `list`, e.g. as is typically output by
  [read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md).
  If `NULL`, will attempt to infer existence of nested metadata

## Value

If `nested_metadata = TRUE` (or is inferred to be `TRUE`), a `list`
containing a `list` containing: 1. a `data.frame` with the pasted data
values from `blocks`, and 2. a `vector` with the pasted metadata values
from `blocks`

If `nested_metadata = FALSE` (or is inferred to be `FALSE`), a `list`
containing `data.frame`'s with the pasted values from `blocks`
