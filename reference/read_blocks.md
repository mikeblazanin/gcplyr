# Read blocks

A function that reads blocks into the R environment

## Usage

``` r
read_blocks(
  files,
  filetype = NULL,
  startrow = NULL,
  endrow = NULL,
  startcol = NULL,
  endcol = NULL,
  sheet = NULL,
  metadata = NULL,
  block_names = NULL,
  block_names_header = "block_name",
  block_names_dot = FALSE,
  block_names_path = TRUE,
  block_names_ext = FALSE,
  header = NA,
  sider = NA,
  wellnames_numeric = FALSE,
  na.strings = c("NA", ""),
  extension,
  block_name_header,
  ...
)
```

## Arguments

- files:

  A `vector` of filepaths relative to the current working directory
  where each filepath is a single plate read

- filetype:

  (optional) the type(s) of the files. Options include:

  "csv", "xls", or "xlsx".

  "tbl" or "table" to use
  [read.table](https://rdrr.io/r/utils/read.table.html) to read the
  file, "csv2" to use
  [read.csv2](https://rdrr.io/r/utils/read.table.html), "delim" to use
  [read.delim](https://rdrr.io/r/utils/read.table.html), or "delim2" to
  use [read.delim2](https://rdrr.io/r/utils/read.table.html).

  If none provided, `read_blocks` will infer filetype(s) from the
  extension(s) in `files`. When extension is not "csv", "xls", or
  "xlsx", will use "table".

- startrow, endrow, startcol, endcol:

  (optional) the rows and columns where the measures data are located in
  `files`.

  Can be a `vector` or `list` the same length as `files`, or a single
  value that applies to all `files`. Values can be `numeric` or a
  `string` that will be automatically converted to `numeric` by
  [from_excel](https://mikeblazanin.github.io/gcplyr/reference/from_excel.md).

  If not provided, data is presumed to begin on the first row and column
  of the file(s) and end on the last row and column of the file(s).

- sheet:

  (optional) If data is in .xls or .xlsx files, which sheet it is
  located on. Defaults to the first sheet if not specified

- metadata:

  (optional) non-density data that should be associated with each read
  blockmeasures. A named `list` where each item in the `list` is either:
  a `vector` of length 2, or a `list` containing two `vector`s.

  In the former case, each `vector` should provide the row and column
  where the metadata is located in all of the blockmeasures input files.

  In the latter case, the first `vector` should provide the rows where
  the metadata is located in each of the corresponding input files, and
  the second `vector` should provide the columns where the metadata is
  located in each of the corresponding input files. (This case is
  typically used when reading multiple blocks from a single file.)

- block_names:

  (optional) `vector` of names corresponding to each plate in `files`.
  If not provided, `block_names` are inferred from the filenames

- block_names_header:

  The name of the metadata field containing the `block_names`

- block_names_dot:

  If `block_names` are inferred from filenames, should the leading './'
  (if any) be retained

- block_names_path:

  If `block_names` are inferred from filenames, should the path (if any)
  be retained

- block_names_ext:

  If `block_names` are inferred from filenames, should the file
  extension (if any) be retained

- header:

  `TRUE`, `FALSE`, or `NA`, or a `vector` of such values, indicating
  whether the file(s) contains the column names as its first line. If
  `header = NA` will attempt to infer the presence of column names. If
  `header = FALSE` or no column names are inferred when `header = NA`,
  column names will be generated automatically according to
  `wellnames_numeric`

- sider:

  `TRUE`, `FALSE`, or `NA`, or a `vector` of such values, indicating
  whether the file(s) contains the row names as its first column. If
  `sider = NA` will attempt to infer the presence of row names. If
  `sider = FALSE` or no row names are inferred when `sider = NA`, row
  names will be generated automatically according to `wellnames_numeric`

- wellnames_numeric:

  If row names and column names are not provided in the input
  `data.frame` as specified by `header` and `sider`, then names will be
  generated automatically according to `wellnames_numeric`.

  If `wellnames_numeric` is `TRUE`, rows and columns will be numbered
  with "R" and "C" prefixes, respectively.

  If `wellnames_numeric` is `FALSE`, rows will be lettered A through Z,
  while columns will be numbered

- na.strings:

  A `character` `vector` of `string`s which are to be interpreted as
  `NA` values by [read.csv](https://rdrr.io/r/utils/read.table.html),
  [read_xls](https://readxl.tidyverse.org/reference/read_excel.html),
  [read_xlsx](https://readxl.tidyverse.org/reference/read_excel.html),
  or [read.table](https://rdrr.io/r/utils/read.table.html)

- extension:

  Deprecated, use `filetype` instead

- block_name_header:

  Deprecated, use `block_names_header` instead

- ...:

  Other arguments passed to
  [read.csv](https://rdrr.io/r/utils/read.table.html),
  [read_xls](https://readxl.tidyverse.org/reference/read_excel.html),
  [read_xlsx](https://readxl.tidyverse.org/reference/read_excel.html),
  or [read.table](https://rdrr.io/r/utils/read.table.html)

## Value

A `list` where each entry is a `list` containing the block `data.frame`
followed by the `block_names` (or filenames, if `block_names` is not
provided) and any specified metadata.

## Details

For `metadata`, `read_blocks` can handle an arbitrary number of
additional pieces of information to extract from each blockcurve file as
metadata. These pieces of information are specified as a named `list` of
`vector`s where each `vector` is the `c(row, column)` where the
information is to be pulled from in the input files.

This metadata is returned as the second `list` element of each
blockcurve, e.g.:

`[[1]]`

`[1]` `data` \#1

`[2]` `metadata`

`[2][1]` `name` \#1

`[2][2]` `date-time` \#1

`[2][3]` `temp` \#1

`[[2]]`

`[1]` `data` \#2

`[2]` `metadata`

`[2][1]` `name` \#2

`[2][2]` `date-time` \#2

`[2][3]` `temp` \#2

`...`

Calling
[uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md)
on the output of `read_blocks` works on block data and the associated
metadata because
[uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md)
operates on the highest level entries of the `list` (the `[[1]]` `[[2]]`
level items), leaving the meta-data associated with the block data

[trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md)
integrates this metadata into the wide-shaped `data.frame` it produces
