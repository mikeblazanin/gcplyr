library(testthat)
library(gcplyr)

test_that("calc_deriv returns correctly with no fitting utilized", {
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

test_that("calc_deriv returns correctly with fitting utilized", {
  #Regular data, deriv
  x <- c(1:10)
  y <- x**2
  expect_equal(calc_deriv(x = x, y = y, window_width_n = 5),
               expected = c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #Regular data, percap
  x <- 0:9
  y <- exp(x)
  calc_deriv(x = x, y = y, percapita = FALSE, blank = 0,
                          window_width_n = 3, trans_y = "log")
  #Can do trans_y = "log" and percapita = FALSE and get right answer
  # Scratch that, answer we get is actually e^right answer
  #Calculations are technically sound for other combos, but I don't
  #  understand conceptually what they're actually showing
  #if y = e^rt, then dy/dt = re^rt -> dy/dt 1/y = r
  #on the other hand, ln(y) = rt
  #So the percapita in linear space is returning correctly in that
  # it's giving us a constant per-capita growth rate, but it's returning
  # incorrectly because the fit is imperfect & so the values are off
  #Whereas the percapita implementation for log space isn't necessary
  # because the log already removes the y from the RHS
  
  x <- seq(from = 0, to = 9, by = 0.01)
  y <- exp(x)
  calc_deriv(x = x, y = y, percapita = TRUE, blank = 0, window_width_n = 3)
  
  #Things work when you have fine enough detailed time steps with linear
  # and percapita = TRUE
  #So now just have to figure out how to do log (and whether can be mixed
  # with percapita)
  
})

