context("Design functions")
library(testthat)
library(gcplyr)

test_that("make_tidydesign issues errors when expected for pattern", {
  expect_error(
    make_tidydesign(nrows = 8, ncols = 12, lookup_tbl_start = "A",
                    "design_elem" = list(c("test1", "test2", "test3"),
                                         2:4,
                                         5:7,
                                         "123")),
    regexp = "Some values in pattern are not in lookup table. Check that you 
             have lookup_tbl_start correct and that you're only using 
             alphanumeric values")
  expect_error(
    make_tidydesign(nrows = 8, ncols = 12, pattern_split = ",",
                    "design_elem" = list(c("test1", "test2", "test3"),
                                         2:4,
                                         5:7,
                                         "1,2,aa")),
    regexp = "Pattern values are multi-character after splitting, but not all pattern values are numeric")
})



#Write regular use test
#Write test that uses 0's
