context("Design functions")
library(testthat)
library(gcplyr)

test_that("make_tidydesign issues errors when expected for invalid pattern strings", {
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

test_that("make_tidydesign works as expected for numerical pattern strings", {
  expect_equal(
    make_tidydesign(nrows = 4, ncols = 4,
                    "name" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "122333")),
    expected = data.frame(Well = paste(rep(1:4, each = 4), rep(LETTERS[1:4], 4),
                                       sep = ""),
                          name = c(NA, NA, NA, NA,
                                   NA, "1", "a", "a",
                                   NA, "hello", "hello", "hello",
                                   NA, NA, NA, NA))
  )
})

test_that("make_tidydesign works with 0's in numerical pattern string", {
  expect_equal(
    make_tidydesign(nrows = 4, ncols = 4,
                  "name" = list(c(1, "a", "hello"),
                                2:3, 2:4,
                                pattern = "122033")),
    expected = data.frame(Well = paste(rep(1:4, each = 4), rep(LETTERS[1:4], 4),
                                       sep = ""),
                          name = c(NA, NA, NA, NA,
                                   NA, "1", "a", "a",
                                   NA, NA, "hello", "hello",
                                   NA, NA, NA, NA))
    )
})

test_that("make_tidydesign works for multiple design elements", {
  expect_equal(
    make_tidydesign(nrows = 4, ncols = 4,
                    "name" = list(c(1, "a", "hello"), 
                                  2:3, 2:4,
                                  pattern = "122033"),
                    "name2" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "233011")),
    expected = data.frame(Well = paste(rep(1:4, each = 4), rep(LETTERS[1:4], 4),
                                       sep = ""),
                          name = c(NA, NA, NA, NA, 
                                   NA, "1", "a", "a",
                                   NA, NA, "hello", "hello",
                                   NA, NA, NA, NA),
                          name2 = c(NA, NA, NA, NA, 
                                    NA, "a", "hello", "hello",
                                   NA, NA, "1", "1",
                                   NA, NA, NA, NA))
  )
})

test_that("make_tidydesign works as expected for alpha pattern strings", {
  expect_equal(
    make_tidydesign(nrows = 4, ncols = 4,
                    lookup_tbl_start = "a",
                    "name" = list(c(1, "a", "hello"),
                                  2:3, 2:4,
                                  pattern = "abbccc")),
    expected = data.frame(Well = paste(rep(1:4, each = 4), rep(LETTERS[1:4], 4),
                                       sep = ""),
                          name = c(NA, NA, NA, NA,
                                   NA, "1", "a", "a",
                                   NA, "hello", "hello", "hello",
                                   NA, NA, NA, NA))
  )
})
