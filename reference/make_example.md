# Create R objects or files as seen in vignette examples

This function makes it easy to generate R objects or files that are
created in the vignette examples. Note that this function should not be
counted on to produce the same output across different versions of
`gcplyr`, as it will be frequently changed to match the examples in the
vignettes.

## Usage

``` r
make_example(vignette, example, dir = ".")
```

## Arguments

- vignette:

  Number of the vignette the example object or file is created in.

- example:

  Number of the example the object or file is created in.

- dir:

  The directory files should be saved into.

## Value

An R object, or the names of the files if files have been written
