context("Design functions")
library(testthat)
library(gcplyr)

test_that("make_tidydesign issues warnings when expected for pattern", {
  
  expect_warning(
    make_tidydesign(nrows = 8, ncols = 12,
                    "design_elem" = list(c("test1", "test2", "test3"),
                                         2:4,
                                         5:7,
                                         "abC")),
    regexp = "Dropping non-consecutive values from pattern lookup table")
  
})
