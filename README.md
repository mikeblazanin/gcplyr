
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots in R chunks. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

# gcplyr

<!-- badges: start -->
<!-- badges: end -->

## What this package can do

`gcplyr` was created to make it easier to import, wrangle, and do
model-free analyses of bacterial growth curve data, as commonly output
by plate readers.

- `gcplyr` can flexibly import all the common data formats output by
  plate readers and reshape them into ‘tidy’ formats for analyses.
- `gcplyr` can import experimental designs from files or directly in
  `R`, then merge this design information with density data.
- This merged tidy-shaped data is then easy to work with and plot using
  functions from `gcplyr` and popular packages `dplyr` and `ggplot2`.
- `gcplyr` can calculate plain and per-capita derivatives of density
  data.
- `gcplyr` has several methods to deal with noise in density or
  derivatives data.
- `gcplyr` can extract parameters like growth rate/doubling time,
  carrying capacity, diauxic shifts, extinction, and more without
  fitting an equation for growth to your data.

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

The best way to get started is to check out [the online
documentation](https://mikeblazanin.github.io/gcplyr/), which includes
examples of all of the most common `gcplyr` functions and walks through
how to import, manipulate, and analyze growth curve data using `gcplyr`
from start to finish.

This documentation is also available as a series of pdf vignette files:

1.  [Introduction](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/gcplyr.pdf)
2.  [Importing and transforming
    data](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/import_transform.pdf)
3.  [Incorporating design
    information](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/incorporate_designs.pdf)
4.  [Pre-processing and plotting
    data](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/preprocess_plot.pdf)
5.  [Processing
    data](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/process.pdf)
6.  [Analyzing
    data](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/analyze.pdf)
7.  [Dealing with
    noise](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/noise.pdf)
8.  [Statistics, merging other data, and other
    resources](https://github.com/mikeblazanin/gcplyr/blob/master/vignettes/conclusion.pdf)

## Citation

Please cite software as:

Blazanin, Michael. 2023. ‘gcplyr: manipulate and analyze growth curve
data.’ R package version 1.1.0
