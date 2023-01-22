library(testthat)
library(gcplyr)

test_that("calc_deriv returns correctly with no fitting utilized", {
  #Regular data, deriv
  x <- c(1:10)
  y <- x**2
  expect_equal(calc_deriv(x = x, y = y),
               expected = c(seq(from = 3, to = 19, by = 2), NA))
  #with x_scale
  expect_equal(calc_deriv(x = x, y = y, x_scale = 10),
               expected = c(seq(from = 3, to = 19, by = 2), NA)*10)
  
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

test_that("calc_deriv returns correctly with fitting utilized", {
  #Regular data, deriv
  x <- c(1:10)
  y <- x**2
  expect_equal(calc_deriv(x = x, y = y, window_width_n = 5),
               expected = c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #Regular data, percap using log transformation
  x <- seq(from = 0, to = 9, by = 1)
  y <- exp(x)
  expect_equal(calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                          window_width_n = 3, trans_y = "log"),
               expected = c(NA, rep(1, length(x)-2), NA))
  #with x_scale
  expect_equal(calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                          window_width_n = 3, trans_y = "log", x_scale = 10),
               expected = c(NA, rep(10, length(x)-2), NA))
  
  #percap using linear (which as time resolution approaches infinity
  # approaches the same value)
  x <- seq(from = 0, to = 9, by = 0.001)
  y <- exp(x)
  expect_equal(tolerance = 0.00001,
               calc_deriv(x = x, y = y, percapita = TRUE, 
                          blank = 0, window_width_n = 3),
               expected = c(NA, rep(1, length(x)-2), NA))
  #with x_scale
  expect_equal(tolerance = 0.00001,
               calc_deriv(x = x, y = y, percapita = TRUE, x_scale = 10,
                          blank = 0, window_width_n = 3),
               expected = c(NA, rep(10, length(x)-2), NA))
  
  expect_error(calc_deriv(x = x, y = y, percapita = FALSE, trans_y = "log",
                          blank = 0, window_width_n = 3))
  expect_error(calc_deriv(x = x, y = y, return = "difference", trans_y = "log",
                          blank = 0, window_width_n = 3))
  
  #Data with NA's
  x <- 1:20
  y <- c(0:5, NA, 7:12, 0, 14:19)
  expect_no_condition(
    calc_deriv(x = x, y = y, percapita = TRUE, window_width_n = 3,
             blank = 0))
  expect_no_condition(
  calc_deriv(x = x, y = y, percapita = TRUE, window_width_n = 3,
             blank = 0, trans_y = "log"))
})

