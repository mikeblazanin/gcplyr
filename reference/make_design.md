# Make design data.frame(s)

This is a function to easily input experimental design elements for
later merging with read data

## Usage

``` r
make_design(
  nrows = NULL,
  ncols = NULL,
  block_row_names = NULL,
  block_col_names = NULL,
  block_name_header = "block_name",
  output_format = "tidy",
  wellnames_numeric = FALSE,
  wellnames_sep = "",
  wellnames_colname = "Well",
  colnames_first = FALSE,
  lookup_tbl_start = 1,
  pattern_split = "",
  ...
)
```

## Arguments

- nrows, ncols:

  Number of rows and columns in the plate data

- block_row_names, block_col_names:

  Names of the rows, columns of the plate blockmeasures data

- block_name_header:

  The name of the field containing the `block_names`

- output_format:

  One of `c("blocks", "blocks_pasted", "wide", "tidy")` denoting the
  format of the resulting `data.frame`

  For easy merging with tidymeasures, leave as default of 'tidy'.

  For human-readability to confirm design is correct, choose 'blocks' or
  'blocks_pasted'.

  For writing to block-shaped file(s), choose 'blocks' or
  'blocks_pasted'.

- wellnames_numeric:

  If `block_row_names` or `block_col_names` are not specified, then
  names will be generated automatically according to
  `wellnames_numeric`.

  If `wellnames_numeric` is `TRUE`, rows and columns will be numbered
  with "R" and "C" prefixes, respectively.

  If `wellnames_numeric` is `FALSE`, rows will be lettered A through Z,
  while columns will be numbered

- wellnames_sep:

  A `string` used when concatenating rownames and column names to create
  well names, when `output_format = "wide"` or `output_format = "tidy"`

- wellnames_colname:

  Header for newly-created column containing the well names, when
  `output_format = "tidy"`

- colnames_first:

  When wellnames are created for `output_format = "wide"` or
  `output_format = "tidy"` by
  [paste](https://rdrr.io/r/base/paste.html)-ing the rownames and column
  names, should the column names come first.

- lookup_tbl_start:

  Value in the lookup table for the split pattern values that
  corresponds to the first value in the `vector`.

  Lookup table by default is `c(1,2,...,8,9,A,B,...Y,Z,a,b,...,y,z)`.
  If, for example, `lookup_tbl_start = "A"`, then the lookup table will
  now be `c(A,B,...Y,Z,a,b,...,y,z)`

- pattern_split:

  character to split pattern elements provided in `...` by, if they're
  not already a `vector`

- ...:

  Each `...` argument must be named, and must be a `list` with five
  elements:

  1.  a `vector` of the values

  2.  a `vector` of the rows the pattern should be applied to

  3.  a `vector` of the columns the pattern should be applied to

  4.  a `string` or `vector` denoting the pattern in which the values
      should be filled into the rows and columns specified.

      If it's a `string`, will be split by `pattern_split`. Pattern will
      be used as the indices of the values `vector`.

      0's refer to `NA`. The pattern will be recycled as necessary to
      fill all the wells of the rows and columns specified.

  5.  a `logical` for whether this pattern should be filled byrow

## Value

Depends on `output_format`:

If `output_format = "blocks"`, a `list` of `data.frame`'s where each
`data.frame` is block-shaped containing the information for a single
design element

If `output_format = "blocks_pasted"`, a single `data.frame` containing
the [paste](https://rdrr.io/r/base/paste.html)-ed information for all
design elements

If `output_format = "wide"`, a wide-shaped `data.frame` containing all
the design elements

If `output_format = "tidy"`, a tidy-shaped `data.frame` containing all
the design elements

## Details

Note that either `nrows` or `block_row_names` must be provided and that
either `ncols` or `block_col_names` must be provided

## Examples

``` r
make_design(nrows = 8, ncols = 12,
            design_element_name = list(c("A", "B", "C"),
                                       2:7,
                                       2:11,
                                       "112301", 
                                       TRUE))
#>    Well design_element_name
#> 1    A1                <NA>
#> 2    A2                <NA>
#> 3    A3                <NA>
#> 4    A4                <NA>
#> 5    A5                <NA>
#> 6    A6                <NA>
#> 7    A7                <NA>
#> 8    A8                <NA>
#> 9    A9                <NA>
#> 10  A10                <NA>
#> 11  A11                <NA>
#> 12  A12                <NA>
#> 13   B1                <NA>
#> 14   B2                   A
#> 15   B3                   A
#> 16   B4                   B
#> 17   B5                   C
#> 18   B6                <NA>
#> 19   B7                   A
#> 20   B8                   A
#> 21   B9                   A
#> 22  B10                   B
#> 23  B11                   C
#> 24  B12                <NA>
#> 25   C1                <NA>
#> 26   C2                <NA>
#> 27   C3                   A
#> 28   C4                   A
#> 29   C5                   A
#> 30   C6                   B
#> 31   C7                   C
#> 32   C8                <NA>
#> 33   C9                   A
#> 34  C10                   A
#> 35  C11                   A
#> 36  C12                <NA>
#> 37   D1                <NA>
#> 38   D2                   B
#> 39   D3                   C
#> 40   D4                <NA>
#> 41   D5                   A
#> 42   D6                   A
#> 43   D7                   A
#> 44   D8                   B
#> 45   D9                   C
#> 46  D10                <NA>
#> 47  D11                   A
#> 48  D12                <NA>
#> 49   E1                <NA>
#> 50   E2                   A
#> 51   E3                   A
#> 52   E4                   B
#> 53   E5                   C
#> 54   E6                <NA>
#> 55   E7                   A
#> 56   E8                   A
#> 57   E9                   A
#> 58  E10                   B
#> 59  E11                   C
#> 60  E12                <NA>
#> 61   F1                <NA>
#> 62   F2                <NA>
#> 63   F3                   A
#> 64   F4                   A
#> 65   F5                   A
#> 66   F6                   B
#> 67   F7                   C
#> 68   F8                <NA>
#> 69   F9                   A
#> 70  F10                   A
#> 71  F11                   A
#> 72  F12                <NA>
#> 73   G1                <NA>
#> 74   G2                   B
#> 75   G3                   C
#> 76   G4                <NA>
#> 77   G5                   A
#> 78   G6                   A
#> 79   G7                   A
#> 80   G8                   B
#> 81   G9                   C
#> 82  G10                <NA>
#> 83  G11                   A
#> 84  G12                <NA>
#> 85   H1                <NA>
#> 86   H2                <NA>
#> 87   H3                <NA>
#> 88   H4                <NA>
#> 89   H5                <NA>
#> 90   H6                <NA>
#> 91   H7                <NA>
#> 92   H8                <NA>
#> 93   H9                <NA>
#> 94  H10                <NA>
#> 95  H11                <NA>
#> 96  H12                <NA>
                          
## To be reminded what arguments are needed, use make_designpattern:
make_design(nrows = 8, ncols = 12,
            design_element_name = make_designpattern(
                 values = c("A", "B", "C"),
                 rows = 2:7, 
                 cols = 2:11,
                 pattern = "112301",
                 byrow = TRUE))              
#>    Well design_element_name
#> 1    A1                <NA>
#> 2    A2                <NA>
#> 3    A3                <NA>
#> 4    A4                <NA>
#> 5    A5                <NA>
#> 6    A6                <NA>
#> 7    A7                <NA>
#> 8    A8                <NA>
#> 9    A9                <NA>
#> 10  A10                <NA>
#> 11  A11                <NA>
#> 12  A12                <NA>
#> 13   B1                <NA>
#> 14   B2                   A
#> 15   B3                   A
#> 16   B4                   B
#> 17   B5                   C
#> 18   B6                <NA>
#> 19   B7                   A
#> 20   B8                   A
#> 21   B9                   A
#> 22  B10                   B
#> 23  B11                   C
#> 24  B12                <NA>
#> 25   C1                <NA>
#> 26   C2                <NA>
#> 27   C3                   A
#> 28   C4                   A
#> 29   C5                   A
#> 30   C6                   B
#> 31   C7                   C
#> 32   C8                <NA>
#> 33   C9                   A
#> 34  C10                   A
#> 35  C11                   A
#> 36  C12                <NA>
#> 37   D1                <NA>
#> 38   D2                   B
#> 39   D3                   C
#> 40   D4                <NA>
#> 41   D5                   A
#> 42   D6                   A
#> 43   D7                   A
#> 44   D8                   B
#> 45   D9                   C
#> 46  D10                <NA>
#> 47  D11                   A
#> 48  D12                <NA>
#> 49   E1                <NA>
#> 50   E2                   A
#> 51   E3                   A
#> 52   E4                   B
#> 53   E5                   C
#> 54   E6                <NA>
#> 55   E7                   A
#> 56   E8                   A
#> 57   E9                   A
#> 58  E10                   B
#> 59  E11                   C
#> 60  E12                <NA>
#> 61   F1                <NA>
#> 62   F2                <NA>
#> 63   F3                   A
#> 64   F4                   A
#> 65   F5                   A
#> 66   F6                   B
#> 67   F7                   C
#> 68   F8                <NA>
#> 69   F9                   A
#> 70  F10                   A
#> 71  F11                   A
#> 72  F12                <NA>
#> 73   G1                <NA>
#> 74   G2                   B
#> 75   G3                   C
#> 76   G4                <NA>
#> 77   G5                   A
#> 78   G6                   A
#> 79   G7                   A
#> 80   G8                   B
#> 81   G9                   C
#> 82  G10                <NA>
#> 83  G11                   A
#> 84  G12                <NA>
#> 85   H1                <NA>
#> 86   H2                <NA>
#> 87   H3                <NA>
#> 88   H4                <NA>
#> 89   H5                <NA>
#> 90   H6                <NA>
#> 91   H7                <NA>
#> 92   H8                <NA>
#> 93   H9                <NA>
#> 94  H10                <NA>
#> 95  H11                <NA>
#> 96  H12                <NA>
```
