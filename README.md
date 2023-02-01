
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!--
You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots in R chunks. In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.
-->

# gcplyr

<!-- badges: start -->
<!-- badges: end -->

`gcplyr` facilitates import, manipulation and model-free analysis of
bacterial growth curve data, as commonly output by plate readers. The
package provides tools for reshaping common plate reader outputs into
‘tidy’ formats and merging them with design information, making data
easy to work with using ‘gcplyr’ and other packages. Also streamlines
common growth curveprocessing steps, like smoothing and calculating
derivatives, and facilitates model-free characterization and analysis of
growth data to extract parameters like growth rate/doubling time,
carrying capacity, diauxic shifts, extinction, and more.

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

You can install gcplyr from
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
how to import, manipulate, and analyze growth curves data using `gcplyr`
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
data.’ R package version 1.0.2
