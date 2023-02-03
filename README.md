
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots in R chunks. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

# gcplyr

<!-- badges: start -->
<!-- badges: end -->

## What this package can do

`gcplyr` facilitates import, manipulation and model-free analysis of
bacterial growth curve data, as commonly output by plate readers.

- Bacterial density time series data is commonly generated by plate
  readers in a number of different formats. `gcplyr` can flexibly import
  all the common formats and reshape them into ‘tidy’ formats for
  downstream analyses.
- Information on the experimental design of a set of growth curves might
  be saved in a file or may only be recorded non-digitally (e.g. in a
  lab notebook). `gcplyr` can import experimental designs from files or
  allow users to input that information directly through `gcplyr`
  functions, then merge this design information with previously-imported
  density data.
- Once data is tidy-formatted and merged with design information, it’s
  easy to work with and plot using functions from `gcplyr` and popular
  packages `dplyr` and `ggplot2`.
- `gcplyr` streamlines a number of different methods to calculate
  derivatives of your density data.
- `gcplyr` also facilitates several strategies to deal with noise in
  your data, including smoothing of raw data and linear fitting during
  derivatives calculation.
- `gcplyr` facilitates model-free characterization and analysis of
  growth data to extract parameters like growth rate/doubling time,
  carrying capacity, diauxic shifts, extinction, and more.

**Please send all questions, requests, comments, and bugs to
mikeblazanin \[at\] gmail \[dot\] com**

## Installation

You can install the latest released version from CRAN by running the
following line in R:

``` r
install.packages("gcplyr")
```

If you would like to install the latest development version from
[GitHub](https://github.com/mikeblazanin/gcplyr/), you can run the
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
