
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.
&#10;You can also embed plots in R chunks. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

# gcplyr

<!-- badges: start -->

[![packageversion](https://img.shields.io/badge/Github-1.12.0-blue.svg?style=flat&logo=github)](commits/master)
[![CRAN
status](https://www.r-pkg.org/badges/version/gcplyr)](https://CRAN.R-project.org/package=gcplyr)
[![License:
MIT](https://img.shields.io/badge/license-MIT-red.svg)](https://cran.r-project.org/web/licenses/MIT)
[![](http://cranlogs.r-pkg.org/badges/grand-total/gcplyr?color=yellow)](https://cran.r-project.org/package=gcplyr)
<!-- badges: end -->

## What this package can do

`gcplyr` was created to make it easier to import, wrangle, and do
model-free analyses of microbial growth curve data, as commonly output
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
  maximum density (carrying capacity), lag time, area under the curve,
  diauxic shifts, extinction, and more without fitting an equation for
  growth to your data.

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

1.  **Introduction: `vignette("gc01_gcplyr")`**
2.  Importing and transforming data: `vignette("gc02_import_reshape")`
3.  Incorporating experimental designs:
    `vignette("gc03_incorporate_designs")`
4.  Pre-processing and plotting your data:
    `vignette("gc04_preprocess_plot")`
5.  Processing your data: `vignette("gc05_process")`
6.  Analyzing your data: `vignette("gc06_analyze")`
7.  Dealing with noise: `vignette("gc07_noise")`
8.  Best practices and other tips: `vignette("gc08_conclusion")`
9.  Working with multiple plates: `vignette("gc09_multiple_plates")`
10. Using make_design to generate experimental designs:
    `vignette("gc10_using_make_design")`

## Citation

Please cite software as:

Blazanin, M. gcplyr: an R package for microbial growth curve data
analysis. BMC Bioinformatics 25, 232 (2024).
<https://doi.org/10.1186/s12859-024-05817-3>
