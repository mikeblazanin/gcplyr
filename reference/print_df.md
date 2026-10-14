# Nicely print the contents of a data.frame

This function uses
[write.table](https://rdrr.io/r/utils/write.table.html) to print the
input `data.frame` in a nicely-formatted manner that is easy to read

## Usage

``` r
print_df(x, col.names = FALSE, row.names = FALSE)
```

## Arguments

- x:

  The `data.frame` to be printed

- col.names:

  Boolean for whether column names should be printed

- row.names:

  Boolean for whether row names should be printed
