context("Utility functions")
library(testthat)
library(growth.curves.pkg)

test_that("checkdim_inputs returns correct length", {
  expect_equal(checkdim_inputs("A", "test_name", needed_len = 5), 
               rep("A", 5))
  expect_equal(checkdim_inputs(rep("A", 5), "test_name", needed_len = 5), 
               rep("A", 5))
})

test_that("checkdim_inputs returns error when appropriate", {
  expect_error(checkdim_inputs(rep("A", 2), "test_name", needed_len = 5))
})

test_that("uninterleave returns correct output", {
  expect_equal(uninterleave(list("A", "B", "C", "D"), n = 1),
               list(list("A", "B", "C", "D")))
  expect_equal(uninterleave(list("A", "B", "C", "D"), n = 2),
               list(list("A", "C"), list("B", "D")))
})

test_that("uninterleave returns error when appropriate", {
  expect_error(uninterleave(list("A", "B", "C", "D"), n = 3))
})

test_that("to_excel returns as expected", {
  expect_identical(to_excel(c(5, 338, 688, 702)), c("E", "LZ", "ZL", "ZZ"))
})

test_that("from_excel returns as expected", {
  expect_identical(from_excel(c("E", "LZ", "ZL", "ZZ")), c(5, 338, 688, 702))
})

test_that("to_excel and from_excel values match each other", {
  expect_equal(from_excel(to_excel(1:10000)), 1:10000)
})

test_that("infer_names works as expected", {
  #Infer but no open cell
  expect_equal(
    infer_names(df = data.frame(matrix(1:16, nrow = 4)),
              startrow = NA, startcol = NA, endrow = NA, endcol = NA,
              infer_colnames = TRUE, infer_rownames = TRUE),
    list(startrow = 1, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #Infer open cell
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = NA, endrow = NA, endcol = NA,
                infer_colnames = TRUE, infer_rownames = TRUE),
    list(startrow = 2, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = 1, colnames_row = 1))
  
  #Infer both, only start row
  expect_equal(
    infer_names(df = data.frame(matrix(c(1:16), nrow = 4)),
                startrow = 2, startcol = NA, endrow = NA, endcol = NA,
                infer_colnames = TRUE, infer_rownames = TRUE),
    list(startrow = 2, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = 1))
  
  #Infer both, only start col
  expect_equal(
    infer_names(df = data.frame(matrix(c(1:16), nrow = 4)),
                startrow = NA, startcol = 2, endrow = NA, endcol = NA,
                infer_colnames = TRUE, infer_rownames = TRUE),
    list(startrow = 1, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = 1, colnames_row = NA))
  
  #No infer, no start info
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = NA, endrow = NA, endcol = NA,
                infer_colnames = FALSE, infer_rownames = FALSE),
    list(startrow = 1, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #No infer, only start col
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = NA, startcol = 2, endrow = NA, endcol = NA,
                infer_colnames = FALSE, infer_rownames = FALSE),
    list(startrow = 1, startcol = 2, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
  
  #No infer, only start row
  expect_equal(
    infer_names(df = data.frame(matrix(c("", 2:16), nrow = 4)),
                startrow = 2, startcol = NA, endrow = NA, endcol = NA,
                infer_colnames = FALSE, infer_rownames = FALSE),
    list(startrow = 2, startcol = 1, endrow = 4, endcol = 4,
         rownames_col = NA, colnames_row = NA))
})