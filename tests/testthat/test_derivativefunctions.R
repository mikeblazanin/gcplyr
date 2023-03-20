library(testthat)
library(gcplyr)

test_that("calc_deriv returns correctly with no fitting utilized", {
  library(dplyr)
  #Regular data, deriv
  dat <- data.frame(x = c(1:10), y = (1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y))$deriv,
               expected = c(seq(from = 3, to = 19, by = 2), NA))
  #with x_scale
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, x_scale = 10))$deriv,
               expected = c(seq(from = 3, to = 19, by = 2), NA)*10)
  
  #Regular data, percap
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))$deriv,
               expected = c(seq(from = 3, to = 19, by = 2)/dat$y[1:9], NA))
  
  #Out of order data, deriv
  dat <- data.frame(x = c(5:10, 1:4), y = c(5:10, 1:4)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y))$deriv,
               expected = c(seq(from = 3, to = 19, by = 2)[dat$x]))
  
  #Out of order data, percap
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))$deriv,
               expected = c(seq(from = 3, to = 19, by = 2)[dat$x]/dat$y))
  
  #data with NAs, deriv
  dat <- data.frame(x = c(1:3, NA, 5:10), y = c((1:6)**2, NA, (8:10)**2),
                    grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y))$deriv,
               expected = c(3, 5, (25-9)/2, NA, 11, (64-36)/2, NA, 17, 19, NA))
  
  #data with NAs, percap
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))$deriv,
               expected = c(3, 5, (25-9)/2, NA, 11, (64-36)/2, NA, 17, 19, NA)/dat$y)
  
  #data all NAs, deriv
  dat <- data.frame(x = 1:10, y = rep(NA, 10), grp = rep("A", 10))
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y))$deriv,
               expected = rep(NA, 10))
  #data all NAs, percap
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, 
                                         percapita = TRUE, blank = 0))$deriv,
               expected = rep(NA, 10))
  
  #data almost all NAs, deriv
  dat <- data.frame(x = 1:10, y = c(NA, NA, NA, NA, 5, rep(NA, 5)), 
                    grp = rep("A", 10))
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y))$deriv,
               expected = rep(NA, 10))
  #data almost all NAs, percap
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, 
                                         percapita = TRUE, blank = 0))$deriv,
               expected = rep(NA, 10))
})

test_that("calc_deriv returns correctly with fitting utilized", {
  library(dplyr)
  #Regular data, deriv
  dat <- data.frame(x = c(1:10), y = c(1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, window_width_n = 5))$deriv,
               expected = c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #Regular data, percap using log transformation
  dat <- data.frame(x = seq(from = 0, to = 9, by = 1), 
                    y = exp(seq(from = 0, to = 9, by = 1)),
                    grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                          window_width_n = 3, trans_y = "log"))$deriv,
               expected = c(NA, rep(1, length(dat$x)-2), NA))
  #with x_scale
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                          window_width_n = 3, trans_y = "log", x_scale = 10))$deriv,
               expected = c(NA, rep(10, length(dat$x)-2), NA))
  
  #percap using linear (which as time resolution approaches infinity
  # approaches the same value)
  dat <- data.frame(x = seq(from = 0, to = 1, by = 0.001), 
                    y = exp(seq(from = 0, to = 1, by = 0.001)),
                    grp = rep("A",length(seq(from = 0, to = 1, by = 0.001))))
  expect_equal(tolerance = 0.00001,
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, 
                          blank = 0, window_width_n = 3))$deriv,
               expected = c(NA, rep(1, length(dat$x)-2), NA))
  #with x_scale
  expect_equal(tolerance = 0.00001,
               mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, percapita = TRUE, x_scale = 10,
                          blank = 0, window_width_n = 3))$deriv,
               expected = c(NA, rep(10, length(dat$x)-2), NA))
  
  expect_error(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, percapita = FALSE, trans_y = "log",
                          blank = 0, window_width_n = 3)))
  expect_error(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, return = "difference", trans_y = "log",
                          blank = 0, window_width_n = 3)))
  
  #Data with NA's
  dat <- data.frame(x = 1:20, y = c(0:5, NA, 7:12, 0, 14:19), grp = rep("A", 20))
  expect_no_error(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, window_width_n = 3,
                              blank = 0)))
  expect_no_warning(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, window_width_n = 3,
             blank = 0)))
  expect_warning(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, window_width_n = 3,
                              blank = 0, trans_y = "log")))
  
  #data all NAs, deriv
  dat <- data.frame(x = 1:10, y = rep(NA, 10), grp = rep("A", 10))
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, window_width_n = 3))$deriv,
               expected = rep(NA, 10))
  #data all NAs, percap
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, window_width_n = 3,
                                         percapita = TRUE, blank = 0))$deriv,
               expected = rep(NA, 10))
  
  #data almost all NAs, deriv
  dat <- data.frame(x = 1:10, y = c(NA, NA, NA, NA, 5, rep(NA, 5)), 
                    grp = rep("A", 10))
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, window_width_n = 3))$deriv,
               expected = rep(NA, 10))
  #data almost all NAs, percap
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, window_width_n = 3,
                                         percapita = TRUE, blank = 0))$deriv,
               expected = rep(NA, 10))
})

test_that("calc_deriv checks for grouping", {
  library(dplyr)
  mydf <- data.frame(x = 1:20, y = sqrt(1:20), 
                     grp = rep(c("A", "B"), each = 10))
  
  expect_warning(calc_deriv(x = mydf$x, y = mydf$y),
                 ".* called outside of dplyr::mutate and subset_by = NULL")
  expect_warning(mutate(mydf,
                        deriv = calc_deriv(x = x, y = y)),
                 ".* called on an ungrouped data.frame and subset_by = NULL")
  expect_no_warning(mutate(group_by(mydf, grp),
                           deriv = calc_deriv(x = x, y = y)))
})
