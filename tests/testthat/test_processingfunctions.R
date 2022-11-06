library(testthat)
library(gcplyr)

test_that("calc_deriv returns correctly", {
  #Regular data, deriv
  x <- c(1:10)
  y <- x**2
  expect_equal(calc_deriv(x = x, y = y),
               expected = c(seq(from = 3, to = 19, by = 2), NA))
  
  #Regular data, percap
  expect_equal(calc_deriv(x = x, y = y, percapita = TRUE, blank = 0),
               expected = c(seq(from = 3, to = 19, by = 2)/y[1:9], NA))
  
  #Out of order data, deriv
  x <- c(5:10, 1:4)
  y <- x**2
  expect_equal(calc_deriv(x = x, y = y),
               expected = c(seq(from = 3, to = 19, by = 2)[x]))
  
  #Out of order data, percap
  expect_equal(calc_deriv(x = x, y = y, percapita = TRUE, blank = 0),
               expected = c(seq(from = 3, to = 19, by = 2)[x]/y))
  
  #data with NAs, deriv
  x <- c(1:3, NA, 5:10)
  y <- c((1:6)**2, NA, (8:10)**2)
  expect_equal(calc_deriv(x = x, y = y),
               expected = c(3, 5, (25-9)/2, NA, 11, (64-36)/2, NA, 17, 19, NA))
  
  #data with NAs, percap
  expect_equal(calc_deriv(x = x, y = y, percapita = TRUE, blank = 0),
               expected = c(3, 5, (25-9)/2, NA, 11, (64-36)/2, NA, 17, 19, NA)/y)
})

