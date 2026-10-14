# Read wides

A function that imports widemeasures in files into the R environment

## Usage

``` r
read_wides(
  files,
  filetype = NULL,
  startrow = NULL,
  endrow = NULL,
  startcol = NULL,
  endcol = NULL,
  header = TRUE,
  sheet = NULL,
  run_names = NULL,
  run_names_header = "file",
  run_names_dot = FALSE,
  run_names_path = TRUE,
  run_names_ext = FALSE,
  metadata = NULL,
  na.strings = c("NA", ""),
  extension,
  names_to_col,
  ...
)
```

## Arguments

- files:

  A `vector` of filepaths (relative to current working directory) where
  each one is a widemeasures set of data

- filetype:

  (optional) the type(s) of the files. Options include:

  "csv", "xls", or "xlsx".

  "tbl" or "table" to use
  [read.table](https://rdrr.io/r/utils/read.table.html) to read the
  file, "csv2" to use
  [read.csv2](https://rdrr.io/r/utils/read.table.html), "delim" to use
  [read.delim](https://rdrr.io/r/utils/read.table.html), or "delim2" to
  use [read.delim2](https://rdrr.io/r/utils/read.table.html).

  If none provided, `read_wides` will infer filetype(s) from the
  extension(s) in `files`. When extension is not "csv", "xls", or
  "xlsx", will use "table".

- startrow, endrow, startcol, endcol:

  (optional) the rows and columns where the data are located in `files`.

  Can be a `vector` or `list` the same length as `files`, or a single
  value that applies to all `files`. Values can be `numeric` or a
  `string` that will be automatically converted to `numeric` by
  [from_excel](https://mikeblazanin.github.io/gcplyr/reference/from_excel.md).

  If not provided, data is presumed to begin on the first row and column
  of the file(s) and end on the last row and column of the file(s).

- header:

  `logical` for whether there is a header in the data. If `FALSE`
  columns are simply numbered. If `TRUE`, the first row of the data
  (`startrow` if specified) is used as the column names

- sheet:

  The sheet of the input files where data is located (if input files are
  .xls or .xlsx). If not specified defaults to the first sheet

- run_names:

  Names to give the widemeasures read in. By default uses the file names
  if not specified

- run_names_header:

  Should the run names (provided in `run_names` or inferred from
  `files`) be added as a column to the widemeasures? If
  `run_names_header` is `NULL`, they will not be. If `run_names_header`
  is a `string`, that `string` will be the column header for the column
  where the names will be stored

- run_names_dot:

  If `run_names` are inferred from filenames, should the leading './'
  (if any) be retained

- run_names_path:

  If `run_names` are inferred from filenames, should the path (if any)
  be retained

- run_names_ext:

  If `run_names` are inferred from filenames, should the file extension
  (if any) be retained

- metadata:

  (optional) non-spectrophotometric data that should be associated with
  each read widemeasures. A named `list` where each item in the `list`
  is either: a `vector` of length 2, or a `list` containing two
  `vector`s.

  In the former case, each `vector` should provide the row and column
  where the metadata is located in all of the blockmeasures input files.

  In the latter case, the first `vector` should provide the rows where
  the metadata is located in each of the corresponding input files, and
  the second `vector` should provide the columns where the metadata is
  located in each of the corresponding input files. (This case is
  typically used when reading multiple blocks from a single file.)

- na.strings:

  A `character` `vector` of `string`s which are to be interpreted as
  `NA` values by [read.csv](https://rdrr.io/r/utils/read.table.html),
  [read_xls](https://readxl.tidyverse.org/reference/read_excel.html),
  [read_xlsx](https://readxl.tidyverse.org/reference/read_excel.html),
  or [read.table](https://rdrr.io/r/utils/read.table.html)

- extension:

  Deprecated, use `filetype` instead

- names_to_col:

  Deprecated, use `run_names_header` instead

- ...:

  Other arguments passed to
  [read.csv](https://rdrr.io/r/utils/read.table.html),
  [read_xls](https://readxl.tidyverse.org/reference/read_excel.html),
  [read_xlsx](https://readxl.tidyverse.org/reference/read_excel.html),
  or [read.table](https://rdrr.io/r/utils/read.table.html)

## Value

A `data.frame` containing a single widemeasures, or A `list` of
widemeasures named by filename

## Details

`startrow`, `endrow`, `startcol`, `endcol`, `sheet` and `filetype` can
either be a single value that applies for all files or `vector`s or
`list`s the same length as `files`,
