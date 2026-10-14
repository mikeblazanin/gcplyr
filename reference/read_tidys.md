# Read tidy-shaped files

A function that imports tidy-shaped files into R. Largely acts as a
wrapper for [read.csv](https://rdrr.io/r/utils/read.table.html),
[read_xls](https://readxl.tidyverse.org/reference/read_excel.html),
[read_xls](https://readxl.tidyverse.org/reference/read_excel.html), or
[read_xlsx](https://readxl.tidyverse.org/reference/read_excel.html), but
can handle multiple files at once and has additional options for taking
subsets of rows/columns rather than the entire file and for adding
filename or run names as an added column in the output.

## Usage

``` r
read_tidys(
  files,
  filetype = NULL,
  startrow = NULL,
  endrow = NULL,
  startcol = NULL,
  endcol = NULL,
  sheet = NULL,
  run_names = NULL,
  run_names_header = NULL,
  run_names_dot = FALSE,
  run_names_path = TRUE,
  run_names_ext = FALSE,
  na.strings = c("NA", ""),
  extension,
  names_to_col,
  ...
)
```

## Arguments

- files:

  A `vector` of filepaths (relative to current working directory) where
  each one is a tidy-shaped data file

- filetype:

  (optional) the type(s) of the files. Options include:

  "csv", "xls", or "xlsx".

  "tbl" or "table" to use
  [read.table](https://rdrr.io/r/utils/read.table.html) to read the
  file, "csv2" to use
  [read.csv2](https://rdrr.io/r/utils/read.table.html), "delim" to use
  [read.delim](https://rdrr.io/r/utils/read.table.html), or "delim2" to
  use [read.delim2](https://rdrr.io/r/utils/read.table.html).

  If none provided, `read_tidys` will infer filetype(s) from the
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

- sheet:

  The sheet of the input files where data is located (if input files are
  .xls or .xlsx). If not specified defaults to the first

- run_names:

  Names to give the tidy files read in. By default uses the file names
  if not specified. These names may be added to the resulting
  `data.frame` depending on the value of the `names_to_col` argument

- run_names_header:

  Should the run names (provided in `run_names` or inferred from
  `files`) be added as a column to the output?

  If `run_names_header` is `TRUE`, they will be added with the column
  name `run_name`

  If `run_names_header` is `FALSE`, they will not be added.

  If `run_names_header` is a `string`, they will be added and the column
  name will be the `string` specified for `run_names_header`.

  If `run_names_header` is `NULL`, they only will be added if there are
  multiple tidy `data.frame`s being read. In which case, the column name
  will be `run_name`

- run_names_dot:

  If `run_names` are inferred from filenames, should the leading './'
  (if any) be retained

- run_names_path:

  If `run_names` are inferred from filenames, should the path (if any)
  be retained

- run_names_ext:

  If `run_names` are inferred from filenames, should the file extension
  (if any) be retained

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
  or [read.table](https://rdrr.io/r/utils/read.table.html) sheet

## Value

A `data.frame` containing a single tidy `data.frame`, or A `list` of
tidy-shaped `data.frame`s named by filename

## Details

`startrow`, `endrow`, `startcol`, `endcol`, `sheet` and `filetype` can
either be a single value that applies for all files or `vector`s or
`list`s the same length as `files`

Note that the `startrow` is always assumed to be a header
