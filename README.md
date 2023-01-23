
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots in R chunks. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

# gcplyr

<!-- badges: start -->
<!-- badges: end -->

`gcplyr` facilitates manipulation and analysis of bacterial growth curve
data.

`gcplyr` is currently in an **Open Beta release**. Most features are
complete and will be relatively stable. Documentation is complete.

**Please send all questions, requests, comments, and bugs to
mikeblazanin \[at\] gmail \[dot\] com**

## What this package can do

- Bacterial density time series data is commonly generated by plate
  readers in a number of different formats. `gcplyr` can flexibly import
  all the common formats and reshape them for downstream analyses.
- Information on the experimental design elements of a set of growth
  curves might be saved in a tabular file (e.g. an Excel file), or may
  only exist non-digitally (e.g. in a lab notebook). `gcplyr` can import
  experimental designs from files or allow users to input that
  information directly through `gcplyr` functions. Then, `gcplyr` can
  merge these design elements with the density data imported from files.
- Growth curves datasets can be processed (e.g. smoothing) and analyzed
  using a variety of `gcplyr` functions

## Installation

You can install gcplyr from [GitHub](https://github.com/) by running the
following lines in R:

``` r
install.packages("devtools")
devtools::install_github("mikeblazanin/gcplyr")
```

## Getting Started

The best way to get started is to check out [the online
documentation](https://mikeblazanin.github.io/gcplyr), which includes
examples of all of the most common `gcplyr` functions and walks through
how to import, manipulate, and analyze growth curves data using `gcplyr`
from start to finish.

This documentation is also available as a series of pdf vignette files:

1.  [Introduction](./vignettes/gcplyr.pdf)
2.  [Importing and transforming data](./vignettes/import_transform.pdf)
3.  [Incorporating design
    information](./vignettes/incorporate_designs.pdf)
4.  [Pre-processing and plotting data](./vignettes/preprocess_plot.pdf)
5.  [Processing data](./vignettes/process.pdf)
6.  [Analyzing data](./vignettes/analyze.pdf)
7.  [Statistics, merging other data, and other
    resources](./vignettes/conclusion.pdf)

## Citation

Please cite software as:

Blazanin, Michael. 2022. ‘gcplyr: manipulate and analyze growth curve
data.’ R package version 0.12.1
