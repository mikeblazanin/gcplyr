library(testthat)
library(gcplyr)

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

test_that("canbe_numeric works as expected", {
  expect_equal(canbe.numeric(c(5, 6, 7)), TRUE)
  
  expect_equal(canbe.numeric(c(5, "6", "7")), TRUE)
  
  expect_equal(canbe.numeric(c("5", "6", "7")), TRUE)
  
  expect_equal(canbe.numeric(c(5, "6", "hello")), FALSE)
  
  expect_error(canbe.numeric(list("a" = 5, "x" = c(6, 7))))
})


test_that("rm_nas returns correctly", {
  expect_equal(rm_nas(x = c(5, 6, NA, 7), na.rm = TRUE), 
               list(x = c(5, 6, 7), y = NULL, nas_indices_removed = 3))
  expect_equal(rm_nas(x = c(5, 6, NA, 7), y = c(5, NA, 6, 7), na.rm = TRUE), 
               list(x = c(5, 7), y = c(5, 7), nas_indices_removed = c(2, 3)))
  expect_equal(rm_nas(x = c(5, 6, 7, 8), y = c(5, NA, 6, 7), na.rm = TRUE), 
               list(x = c(5, 7, 8), y = c(5, 6, 7), nas_indices_removed = 2))
  expect_equal(rm_nas(x = c(5, 6, 7, NA), y = c(5, 6, 7, 8), na.rm = TRUE), 
               list(x = c(5, 6, 7), y = c(5, 6, 7), nas_indices_removed = 4))
})

test_that("add_nas returns correctly", {
  expect_equal(add_nas(x = c(5, 6, 7, 8), nas_indices_removed = 3), 
               list(x = c(5, 6, NA, 7, 8), y = NULL))
  expect_equal(add_nas(x = c(5, 6, 7, 8), y = 1:4, nas_indices_removed = 3), 
               list(x = c(5, 6, NA, 7, 8), y = c(1, 2, NA, 3, 4)))
})

test_that("reorder returns correctly", {
  expect_equal(reorder(x = c(5, 6, 7, 8), y = c(1, 5, 3, 2)), 
               list(x = c(5, 6, 7, 8), y = c(1, 5, 3, 2), order = 1:4))
  expect_equal(reorder(x = c(5, 8, 2, 4), y = 1:4), 
               list(x = c(2, 4, 5, 8), y = c(3, 4, 1, 2), order = c(3, 4, 1, 2)))
})