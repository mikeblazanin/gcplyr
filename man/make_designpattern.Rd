% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{make_designpattern}
\alias{make_designpattern}
\title{Make design pattern}
\usage{
make_designpattern(values, rows, cols, pattern, byrow = TRUE)
}
\arguments{
\item{values}{Vector of values to use}

\item{rows}{Vector of rows where pattern applies}

\item{cols}{Vector of cols where pattern applies}

\item{pattern}{Numeric pattern itself, where numbers refer to entries
in \code{values}}

\item{byrow}{Boolean for whether pattern should be created by row}
}
\description{
A helper function for use with make_tidydesign
}
\details{
Example:
my_example <- make_tidydesign(nrows = 8, ncols = 12,
      design_element_name = make_designpattern(values = c("L", "G", "C"),
                                                rows = 2:7, cols = 2:11,
                                                pattern = "11223300",
                                                byrow = TRUE))
}