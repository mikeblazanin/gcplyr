# Import blockdesigns

Function to import block-shaped designs from files and return tidy
designs. This function acts as a wrapper that calls
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
[paste_blocks](https://mikeblazanin.github.io/gcplyr/reference/paste_blocks.md),
[trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md),
[trans_wide_to_tidy](https://mikeblazanin.github.io/gcplyr/reference/trans_wide_to_tidy.md),
and
[separate_tidy](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md)

## Usage

``` r
import_blockdesigns(
  files,
  block_names = NULL,
  block_name_header = "block_name",
  join_as_cols = TRUE,
  sep = NULL,
  values_colname = "Designs",
  into = NULL,
  keep_blocknames = !join_as_cols,
  warn_joinrows_nointo = TRUE,
  join_designs = NULL,
  ...
)
```

## Arguments

- files:

  A `vector` of filepaths relative to the current working directory
  where each filepath is a single plate read to be read by
  [read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md).

- block_names:

  `vector` of names corresponding to each design element (each block).
  Inferred from filenames, if not specified.

  When `keep_blocknames = TRUE`, the output will have a column
  containing these values, with the column name specified by
  `block_name_header`.

  When `join_as_cols = TRUE`, the `block_names` are also used as the
  output column names for each separated design column.

- block_name_header:

  When `keep_blocknames = TRUE`, the column name of the column
  containing the `block_names`.

- join_as_cols:

  `logical` indicating whether blocks (if there are multiple) should be
  joined as columns (i.e. describe the same plate) in the tidy output.
  If `FALSE`, blocks are joined as rows (i.e. describe different plates)
  in the tidy output.

- sep:

  If designs have been pasted together, this specifies the `string` they
  should be split apart by via
  [separate_tidy](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md).

- values_colname:

  When `join_as_cols = FALSE` and `sep` is not specified, all the design
  values will be in a column named by `values_colname`. For other cases,
  see the **Value** section.

- into:

  When `sep` is specified, `into` sets the names of the columns after
  splitting (see **Value** section for behavior when `into` is not set).

- keep_blocknames:

  `logical` indicating whether the column containing `block_names` (or
  those inferred from file names) should be retained in the output. By
  default, blocknames are retained only if `join_as_cols = FALSE`.

- warn_joinrows_nointo:

  `logical` indicating whether warning should be raised when multiple
  blocks are joined as rows (`join_as_cols = FALSE`) and `sep` is
  specified, but `into` is not specified.

- join_designs:

  Deprecated, use `join_as_cols` instead

- ...:

  Other arguments to pass to
  [read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
  [paste_blocks](https://mikeblazanin.github.io/gcplyr/reference/paste_blocks.md),
  [trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md),
  [trans_wide_to_tidy](https://mikeblazanin.github.io/gcplyr/reference/trans_wide_to_tidy.md),
  or
  [separate_tidy](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md).

  See Details for more information

## Value

A tidy-shaped `data.frame` containing the design information from
`files`. This always includes a `Well` column.

If `keep_blocknames = TRUE`, this includes a column with the column name
specified by `block_name_header` and containing `block_names` (or block
names inferred from file names).

The layout of the design values varies depending on the inputs:

If `join_as_cols = TRUE`, each block was joined as a column, with the
columns named according to `block_names` (or block names inferred from
file names). In this case, if `sep` was specified, each column was split
by `sep` into columns named by splitting the corresponding block name by
`sep` (post-split column names can alternatively be specified directly
via `into`).

Otherwise, when `join_as_cols = FALSE`, each block was joined as rows,
with the column containing all design values named by `values_colname`.
In this case, if `sep` was specified, that single design column was
split by `sep` into columns named by splitting `values_colname`
(post-split column names can alternatively be specified directly via
`into`).

## Details

Other common arguments that you may want to provide via `...` include:

`startrow`, `endrow`, `startcol`, `endcol`, `sheet` - specifying the
location of design information inside `files` to
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md).

`wellnames_sep` - specifying what character (or "" for none) should be
used when pasting together the rownames and column names. Note that this
should be chosen to match the well names in your measures.

`into` - specifying the column names resulting from using
[separate_tidy](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md)
on the `values_colname` column.

Note that `import_blockdesigns` cannot currently handle metadata
specified via the `metadata` argument of
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md).

If you find yourself needing more control, you can run the steps
manually, first reading with
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
pasting as needed with
[paste_blocks](https://mikeblazanin.github.io/gcplyr/reference/paste_blocks.md),
transforming to tidy with
[trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md)
and
[trans_wide_to_tidy](https://mikeblazanin.github.io/gcplyr/reference/trans_wide_to_tidy.md),
and separating as needed with
[separate_tidy](https://mikeblazanin.github.io/gcplyr/reference/separate_tidy.md).
