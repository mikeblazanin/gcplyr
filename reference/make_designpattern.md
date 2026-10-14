# Make design pattern

A helper function for use with
[make_design](https://mikeblazanin.github.io/gcplyr/reference/make_design.md)

## Usage

``` r
make_designpattern(
  values,
  rows,
  cols,
  pattern = 1:length(values),
  byrow = TRUE
)

mdp(values, rows, cols, pattern = 1:length(values), byrow = TRUE)
```

## Arguments

- values:

  `vector` of values to use

- rows:

  `vector` of rows where pattern applies

- cols:

  `vector` of cols where pattern applies

- pattern:

  `numeric` pattern itself, where numbers refer to entries in `values`

- byrow:

  `logical` for whether pattern should be created by row

## Value

`list(values, rows, cols, pattern, byrow)`

## See also

[make_design](https://mikeblazanin.github.io/gcplyr/reference/make_design.md)

## Examples

``` r
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
