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