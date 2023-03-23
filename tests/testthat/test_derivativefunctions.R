library(testthat)
library(gcplyr)

test_that("calc_deriv on regular data, no fitting", {
  library(dplyr)
  #percapita = FALSE
  dat <- data.frame(x = c(1:10), y = (1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y))$deriv,
    expected = c(seq(from = 3, to = 19, by = 2), NA))
  
  #percapita = TRUE, trans_y = 'linear'
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))$deriv,
    expected = c(seq(from = 3, to = 19, by = 2)/dat$y[1:9], NA))
  
  #percapita = TRUE, trans_y = 'log'
  dat <- data.frame(x = 1:10, y = exp(1:10), grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp), 
           percap = calc_deriv(x = x, y = y, percapita = TRUE,
                                    trans_y = 'log', blank = 0))$percap,
    c(rep(1, 9), NA))
})

test_that("calc_deriv on regular data, fitting", {
  library(dplyr)
  
  #percapita = FALSE
  dat <- data.frame(x = c(1:10), y = c(1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, window_width_n = 5))$deriv,
    expected = c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #percapita = TRUE, trans_y = 'linear'
  #(which as time resolution approaches infinity approaches the same value)
  dat <- data.frame(x = seq(from = 0, to = .1, by = 0.001), 
                    y = exp(seq(from = 0, to = .1, by = 0.001)),
                    grp = rep("A",length(seq(from = 0, to = .1, by = 0.001))))
  expect_equal(tolerance = 0.00001,
               mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, percapita = TRUE, 
                                         blank = 0, window_width_n = 3))$deriv,
               expected = c(NA, rep(1, length(dat$x)-2), NA))
  
  #percapita = TRUE, trans_y = 'log'
  dat <- data.frame(x = seq(from = 0, to = 9, by = 1), 
                    y = exp(seq(from = 0, to = 9, by = 1)),
                    grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                              window_width_n = 3, trans_y = "log"))$deriv,
    expected = c(NA, rep(1, length(dat$x)-2), NA))
})

test_that("calc_deriv with x_scale", {
  library(dplyr)
  
  #with no fitting
  dat <- data.frame(x = c(1:10), y = (1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, x_scale = 10))$deriv,
    expected = c(seq(from = 3, to = 19, by = 2), NA)*10)
  
  #with no fitting, percap, linear
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, 
                              blank = 0, x_scale = 7))$deriv,
    expected = 7*c(seq(from = 3, to = 19, by = 2)/dat$y[1:9], NA))
  
  #with no fitting, percap, log
  dat <- data.frame(x = 1:10, y = exp(1:10), grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp), 
           percap = calc_deriv(x = x, y = y, percapita = TRUE,
                               trans_y = 'log', blank = 0, x_scale = 3))$percap,
    c(rep(3, 9), NA))
  
  #with fitting
  dat <- data.frame(x = c(1:10), y = c(1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, window_width_n = 5,
                              x_scale = 9))$deriv,
    expected = 9*c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #with fitting, percap, linear
  dat <- data.frame(x = seq(from = 0, to = .1, by = 0.001), 
                    y = exp(seq(from = 0, to = .1, by = 0.001)),
                    grp = rep("A",length(seq(from = 0, to = .1, by = 0.001))))
  expect_equal(tolerance = 0.00001,
               mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, percapita = TRUE, x_scale = 10,
                                         blank = 0, window_width_n = 3))$deriv,
               expected = c(NA, rep(10, length(dat$x)-2), NA))
  
  #with fitting, percap, log
  dat <- data.frame(x = seq(from = 0, to = 9, by = 1), 
                    y = exp(seq(from = 0, to = 9, by = 1)),
                    grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0,
                              window_width_n = 3, trans_y = "log",
                              x_scale = 6))$deriv,
    expected = 6*c(NA, rep(1, length(dat$x)-2), NA))
})

test_that("calc_deriv with non-zero blank", {
  library(dplyr)
  
  #with no fitting
  dat <- data.frame(x = 1:10, y = 7+(1:10)**2, grp = rep("A", 10))
  expect_equal(mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, blank = 7))$deriv,
               c(seq(from = 3, to = 19, by = 2), NA))
  
  #with no fitting, percap, linear
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE,  blank = 7))$deriv,
    expected = c(seq(from = 3, to = 19, by = 2)/(dat$y[1:9]-7), NA))
  
  #with no fitting, percap, log
  dat <- data.frame(x = 1:10, y = 4+exp(1:10), grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp), 
           percap = calc_deriv(x = x, y = y, percapita = TRUE,
                               trans_y = 'log', blank = 4))$percap,
    c(rep(1, 9), NA))
  
  #with fitting
  dat <- data.frame(x = c(1:10), y = 3+c(1:10)**2, grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, window_width_n = 5,
                              blank = 3))$deriv,
    expected = c(NA, NA, seq(from = 6, to = 16, by = 2), NA, NA))
  
  #with fitting, percap, linear
  dat <- data.frame(x = seq(from = 0, to = .1, by = 0.001), 
                    y = 2+exp(seq(from = 0, to = .1, by = 0.001)),
                    grp = rep("A",length(seq(from = 0, to = .1, by = 0.001))))
  expect_equal(tolerance = 0.00001,
               mutate(group_by(dat, grp),
                      deriv = calc_deriv(x = x, y = y, percapita = TRUE, 
                                         x_scale = 10,
                                         blank = 2, window_width_n = 3))$deriv,
               expected = c(NA, rep(10, length(dat$x)-2), NA))
  
  #with fitting, percap, log
  dat <- data.frame(x = seq(from = 0, to = 9, by = 1), 
                    y = 5+exp(seq(from = 0, to = 9, by = 1)),
                    grp = rep("A", 10))
  expect_equal(
    mutate(group_by(dat, grp),
           deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 5,
                              window_width_n = 3, trans_y = "log",
                              x_scale = 6))$deriv,
    expected = 6*c(NA, rep(1, length(dat$x)-2), NA))
})

test_that("calc_deriv on 'weird' data, no fitting", {
  library(dplyr)
  
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

test_that("calc_deriv on 'weird' data, fitting", {
  library(dplyr)
  
  #Errors when trans_y incompatible with percapita or return
  dat <- data.frame(x = seq(from = 0, to = 1, by = 0.001), 
                    y = exp(seq(from = 0, to = 1, by = 0.001)),
                    grp = rep("A",length(seq(from = 0, to = 1, by = 0.001))))
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
