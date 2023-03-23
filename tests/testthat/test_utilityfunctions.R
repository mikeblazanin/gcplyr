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
  
  expect_equal(canbe.numeric("INF"), TRUE)
  expect_equal(canbe.numeric("NAN"), TRUE)
  expect_equal(canbe.numeric("INF", infinite_num = FALSE), FALSE)
  expect_equal(canbe.numeric("NAN", infinite_num = FALSE), TRUE)
  expect_equal(canbe.numeric("INF", nan_num = FALSE), TRUE)
  expect_equal(canbe.numeric("NAN", nan_num = FALSE), FALSE)
})


test_that("rm_nas returns correctly", {
  expect_equal(rm_nas(x = c(5, 6, NA, 7), y = NULL, na.rm = TRUE), 
               list(x = c(5, 6, 7), y = NULL, nas_indices_removed = 3))
  expect_equal(rm_nas(x = c(5, 6, NA, 7), y = c(5, NA, 6, 7), na.rm = TRUE), 
               list(x = c(5, 7), y = c(5, 7), nas_indices_removed = c(2, 3)))
  expect_equal(rm_nas(x = c(5, 6, 7, 8), y = c(5, NA, 6, 7), na.rm = TRUE), 
               list(x = c(5, 7, 8), y = c(5, 6, 7), nas_indices_removed = 2))
  expect_equal(rm_nas(x = c(5, 6, 7, NA), y = c(5, 6, 7, 8), na.rm = TRUE), 
               list(x = c(5, 6, 7), y = c(5, 6, 7), nas_indices_removed = 4))
  expect_equal(rm_nas(x = c(5, 6, 7, NA), y = c(5, 6, 7, 8),
                      z = c(5, NA, 8, 9), na.rm = TRUE), 
               list(x = c(5, 7), y = c(5, 7), z = c(5, 8),
                    nas_indices_removed = c(2, 4)))
})

test_that("add_nas returns correctly", {
  expect_equal(add_nas(x = c(5, 6, 7, 8), nas_indices_removed = 3), 
               list(x = c(5, 6, NA, 7, 8), y = NULL))
  expect_equal(add_nas(x = c(5, 6, 7, 8), y = 1:4, nas_indices_removed = 3), 
               list(x = c(5, 6, NA, 7, 8), y = c(1, 2, NA, 3, 4)))
})

test_that("reorder_xy returns correctly", {
  expect_equal(reorder_xy(x = c(5, 6, 7, 8), y = c(1, 5, 3, 2)), 
               list(x = c(5, 6, 7, 8), y = c(1, 5, 3, 2), order = 1:4))
  expect_equal(reorder_xy(x = c(5, 8, 2, 4), y = 1:4), 
               list(x = c(2, 4, 5, 8), y = c(3, 4, 1, 2), order = c(3, 4, 1, 2)))
})

test_that("get_windows returns correctly", {
  expect_equal(
    get_windows(x = 1:10, y = 1:10, window_width_n = 3, edge_NA = FALSE),
    list(c(1, 2), c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5, 6),
         c(5, 6, 7), c(6, 7, 8), c(7, 8, 9), c(8, 9, 10), c(9, 10)))
  expect_equal(
    get_windows(x = c(1:5, 7:11), y = 1:10, window_width = 2, edge_NA = FALSE),
    list(c(1, 2), c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5),
         c(6, 7), c(6, 7, 8), c(7, 8, 9), c(8, 9, 10), c(9, 10)))
  expect_equal(
    get_windows(x = 1:10, y = c(1:5, 7:11), window_height = 1, edge_NA = FALSE),
    list(c(1, 2), c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5),
         c(6, 7), c(6, 7, 8), c(7, 8, 9), c(8, 9, 10), c(9, 10)))
  expect_equal(
    get_windows(x = 1:10, y = 1:10, window_width_n = 3, edge_NA = TRUE),
    list(NA, c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5, 6),
         c(5, 6, 7), c(6, 7, 8), c(7, 8, 9), c(8, 9, 10), NA))
  expect_equal(
    get_windows(x = c(1:5, 7:11), y = 1:10, window_width = 2, edge_NA = TRUE),
    list(NA, c(1, 2, 3), c(2, 3, 4), c(3, 4, 5), c(4, 5),
         c(6, 7), c(6, 7, 8), c(7, 8, 9), c(8, 9, 10), NA))
})

test_that("solve_linear returns correctly", {
  expect_equal(solve_linear(x1 = 0, y1 = 0, x2 = 5, y2 = 5), setNames(1, "m"))
  expect_equal(solve_linear(x1 = 0, y1 = 0, x2 = 5, m = 1), setNames(5, "y2"))
  expect_equal(solve_linear(x1 = 0, y1 = 0, y2 = 5, m = 2), setNames(2.5, "x2"))
  expect_equal(solve_linear(x1 = 0, y1 = 0, x2 = 10, y2 = 5, x3 = 15), 
               setNames(7.5, "y3"))
  expect_equal(solve_linear(x1 = 0, y1 = 0, x2 = 10, y2 = 5, y3 = 7.5), 
               setNames(15, "x3"))
})
  
