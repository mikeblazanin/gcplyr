# Import blockmeasures

Function to import blockmeasures from files and return widemeasures.
This function acts as a wrapper to call
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
[uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md),
then
[trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md)
in one go.

## Usage

``` r
import_blockmeasures(
  files,
  num_plates = 1,
  plate_names = NULL,
  wellnames_sep = "",
  ...
)
```

## Arguments

- files:

  `vector` of filenames (as `string`s), each of which is a block-shaped
  file containing measures data. File formats can be .csv, .xls, or
  .xlsx

- num_plates:

  Number of plates. If multiple plates
  [uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md)
  will be used to separate blockmeasures into those plates accordingly

- plate_names:

  (optional) Names to put onto the plates when output

- wellnames_sep:

  `string` to use as separator for well names between rowname and column
  name

- ...:

  Other arguments to pass to
  [read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
  [uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md),
  or
  [trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md)

## Value

If `num_plates = 1`, a wide-shaped `data.frame` containing the measures
data.

if `num_plates` is greater than one, a `list` of `data.frame`'s, where
each `data.frame` is wide-shaped.

## Details

Common arguments that you may want to provide via `...` include:

`startrow`, `endrow`, `startcol`, `endcol`, `sheet` - specifying the
location of design information inside `files` to
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md)

`metadata` - specifying metadata to
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md)

See
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md)
for more details

If you find yourself needing more control, you can run the steps
manually, first reading with
[read_blocks](https://mikeblazanin.github.io/gcplyr/reference/read_blocks.md),
separating plates as needed with
[uninterleave](https://mikeblazanin.github.io/gcplyr/reference/uninterleave.md),
then transforming to wide with
[trans_block_to_wide](https://mikeblazanin.github.io/gcplyr/reference/trans_block_to_wide.md).
