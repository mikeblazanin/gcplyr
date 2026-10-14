# gcplyr

## What this package can do

`gcplyr` makes it easier to import, wrangle, and do model-free analyses
of microbial growth curve data, as commonly output by plate readers.

- `gcplyr` can flexibly import all the common data formats output by
  plate readers and reshape them into ‘tidy’ formats for analyses.
- `gcplyr` can import experimental designs from files or generate them
  in `R`, and can merge experimental design information with density
  data from plate readers.
- `gcplyr` makes it easy to analyze growth curve data with `tidyverse`
  or `dplyr` functions, and to plot growth curve data with `ggplot2`.
- `gcplyr` can extract parameters like growth rate/doubling time,
  maximum density (carrying capacity), lag time, area under the curve,
  diauxic shifts, extinction, and more without fitting an equation for
  growth to your data.
- `gcplyr` has several methods to deal with noise in growth curve data.

**Please send all questions, requests, comments, and bugs to
mikeblazanin \[at\] gmail \[dot\] com**

## Installation

You can install the version most-recently released on CRAN by running
the following line in R:

``` r

install.packages("gcplyr")
```

You can install the most recently-released version from
[GitHub](https://github.com/mikeblazanin/gcplyr/) by running the
following lines in R:

``` r

install.packages("devtools")
devtools::install_github("mikeblazanin/gcplyr")
```

## Getting Started

The best way to get started is to read through the articles series,
which breaks down a typical workflow using `gcplyr` from start to
finish, starting with the introduction:

1.  **Introduction:
    [`vignette("gc01_gcplyr")`](https://mikeblazanin.github.io/gcplyr/articles/gc01_gcplyr.md)**
2.  Importing and transforming data:
    [`vignette("gc02_import_reshape")`](https://mikeblazanin.github.io/gcplyr/articles/gc02_import_reshape.md)
3.  Incorporating experimental designs:
    [`vignette("gc03_incorporate_designs")`](https://mikeblazanin.github.io/gcplyr/articles/gc03_incorporate_designs.md)
4.  Pre-processing and plotting your data:
    [`vignette("gc04_preprocess_plot")`](https://mikeblazanin.github.io/gcplyr/articles/gc04_preprocess_plot.md)
5.  Processing your data:
    [`vignette("gc05_process")`](https://mikeblazanin.github.io/gcplyr/articles/gc05_process.md)
6.  Analyzing your data:
    [`vignette("gc06_analyze")`](https://mikeblazanin.github.io/gcplyr/articles/gc06_analyze.md)
7.  Dealing with noise:
    [`vignette("gc07_noise")`](https://mikeblazanin.github.io/gcplyr/articles/gc07_noise.md)
8.  Best practices and other tips:
    [`vignette("gc08_conclusion")`](https://mikeblazanin.github.io/gcplyr/articles/gc08_conclusion.md)
9.  Working with multiple plates:
    [`vignette("gc09_multiple_plates")`](https://mikeblazanin.github.io/gcplyr/articles/gc09_multiple_plates.md)
10. Using make_design to generate experimental designs:
    [`vignette("gc10_using_make_design")`](https://mikeblazanin.github.io/gcplyr/articles/gc10_using_make_design.md)

## Citation

Please cite software as:

Blazanin, M. gcplyr: an R package for microbial growth curve data
analysis. BMC Bioinformatics 25, 232 (2024).
<https://doi.org/10.1186/s12859-024-05817-3>
