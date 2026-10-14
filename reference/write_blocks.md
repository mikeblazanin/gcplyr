# Write block designs to csv

This function writes block-shaped `list`s (as created by
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md)
or
[make_design](https://mikeblazanin.github.io/gcplyr/reference/make_design.md))
to csv files, including both `data` and `metadata` in a variety of
output formats

## Usage

``` r
write_blocks(
  blocks,
  file,
  output_format = "multiple",
  block_name_location = NULL,
  block_name_header = "block_name",
  paste_sep = "_",
  filename_sep = "_",
  na = "",
  dir = NULL,
  ...
)
```

## Arguments

- blocks:

  `list` of block-shaped data to be written to file

- file:

  `NULL`, a `character` `string` naming a file to write to, or a
  `vector` of `character` `string`s naming files to write to.

  A file name is required when `output_format = "single"`

  A file name can be specified when `output_format = "pasted"`, or
  `file` can be set to `NULL` as long as
  `block_name_location = "filename"` (where pasted `block_name` metadata
  will be used for the file name)

  File names can be specified when `output_format = "multiple"`, or
  `file` can be set to `NULL` as long as
  `block_name_location = "filename"` (where the `block_name` metadata
  will be used for the file names)

- output_format:

  One of "single", "pasted", "multiple".

  "single" will write all blocks into a single csv file, with an empty
  row between successive blocks.

  "pasted" will paste all blocks together using a `paste_sep`, and then
  write that now-pasted block to a single csv file.

  "multiple" will write each block to its own csv file.

- block_name_location:

  Either `NULL`, 'filename' or 'file'.

  If `NULL`, `block_name_location` will be automatically selected based
  on `output_format`. For `output_format = 'single'` and
  `output_format = 'pasted'`, `block_name_location` defaults to 'file'.
  For `output_format = 'multiple'`, `block_name_location` defaults to
  'filename'

  If 'filename', the `block_name` metadata will be used as the output
  file name(s) when no file name(s) are provided, or appended to file
  name(s) when they have been provided.

  If 'file', the `block_name` metadata will be included as a row in the
  output file.

- block_name_header:

  The name of the field containing the `block_names`

- paste_sep:

  When `output_format = 'pasted'`, what character will be used to paste
  together blocks.

- filename_sep:

  What character will be used to paste together filenames when
  `block_name_location = 'filename'`.

- na:

  The `string` to use for missing values in the data.

- dir:

  The directory that file(s) will be written into. When `dir = NULL`,
  writes to the current working directory. (Can only be used when
  `file = NULL`)

- ...:

  Other arguments passed to
  [write.table](https://rdrr.io/r/utils/write.table.html)

## Value

Nothing, but R objects are written to files
