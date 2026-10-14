# Design for example growth curve data

A tidy-shaped dataset with the experimental design (i.e. plate layout)
for the example data included with `gcplyr`.

## Usage

``` r
example_design_tidy
```

## Format

A `data.frame` with 96 rows and 3 variables:

- `Well`: The well of the plate

- `Bacteria_strain`: The numbered bacterial strain growing in each well

- `Phage`: Whether or not the bacteria were simulated growing with
  phages

## Details

Wells `A1...A8` through `F1...F8` contain 48 different simulated
bacterial strains growing alone. Wells `G1...G8` through `L1...L8`
contain the same 48 bacterial strains in an identical layout, but this
time growing in the presence of a phage
