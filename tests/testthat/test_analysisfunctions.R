library(testthat)
library(gcplyr)

test_that("auc returns correctly with no xlim", {
  expect_equal(auc(x = 1:10, y = (1:10)**2), 669/2)
  expect_equal(auc(x = c(1:5, NA, 6:10), y = c((1:5)**2, NA, (6:10)**2)), 669/2)
  expect_equal(auc(x = c(1:3, NA, 4:10), y = c((1:7)**2, NA, (8:10)**2)), 375)
  expect_equal(auc(x = c(1:10), y = c(1:5, 5:1)), 29)
})

test_that("auc returns correctly with xlim", {
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(2, 9)), 241.5)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(1, 9)), 244)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(4, 10)), 313)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(3.5, 7.5)), 127)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(2, NA)), 332)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(NA, 7)), 115)
  expect_error(auc(x = 1:10, y = (1:10)**2, xlim = c(NA, NA)))
})

test_that("first_peak matches find_local_extrema results", {
  expect_equal(
    first_peak(y = (20 - abs(12 - 1:20))),
    find_local_extrema(y = (20 - abs(12 - 1:20)),
                       return_minima = FALSE,
                       return_endpoints = FALSE,
                       width_limit_n = 3))
  expect_equal(
    first_peak(y = (20 - abs(12 - 1:20)), return = "y"), 20)
  expect_equal(
    first_peak(x = 21:40, y = (20 - abs(12 - 1:20)), return = "x"), 32)
})

test_that("first_below works correctly with no subset", {
  dat <- data.frame(x = 1:20,
                    y = 20 - abs(12 - 1:20))
  expect_equal(first_below(y = 20:1, threshold = 15), 6)
  expect_equal(first_below(y = 20:1, threshold = 10.5), 11)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", threshold = 10), 31)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", threshold = 10.5), 30.5)
})

test_that("first_below works correctly with subset", {
  dat <- data.frame(x = 1:20,
                    y = 20 - abs(12 - 1:20))
  expect_equal(first_below(y = 20:1, threshold = 15, subset = (1:20 < 10)), 6)
  expect_equal(
    first_below(y = 20:1, threshold = 10.5, subset = (abs(1:20-10) < 5)), 11)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", 
                threshold = 10, subset = (abs(1:20-10) < 5)), 31)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", 
                threshold = 10.5, subset = (abs(1:20-10) < 5)), 30.5)
})

test_that("find_threshold_crosses works correctly", {
  #data in order
  dat <- data.frame(x = 1:40,
                    y = rep(20 - abs(12 - 1:20), 2))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = TRUE, return_falling = FALSE,
                           threshold = 16.5, return = "index"),
    c(9, 29))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = FALSE, return_falling = TRUE,
                           return_endpoints = TRUE,
                           threshold = 16.5, return = "index"),
    c(1, 16, 36))
                           
  #data out of order
  dat <- dat[c(27:40, 1:26), ]
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = TRUE, return_falling = FALSE,
                           threshold = 16.5, return = "index"),
    c(3, 23))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = FALSE, return_falling = TRUE,
                           return_endpoints = TRUE,
                           threshold = 16.5, return = "index"),
    c(10, 15, 30))
  
  #data with NA's
  dat$x[7] <- NA
  dat$y[26] <- NA
  
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = TRUE, return_falling = FALSE,
                           threshold = 16.5, return = "index"),
    c(3, 23))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = FALSE, return_falling = TRUE,
                           return_endpoints = TRUE,
                           threshold = 16.5, return = "index"),
    c(10, 15, 30))
  
  #data with subset specified
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = TRUE, return_falling = FALSE,
                           threshold = 16.5, return = "index",
                           subset = dat$x > 15),
    c(3))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = FALSE, return_falling = TRUE,
                           return_endpoints = TRUE,
                           threshold = 16.5, return = "index",
                           subset = dat$x < 25),
    c(15, 30))
})

