library(testthat)
library(gcplyr)

test_that("auc returns correctly with no xlim", {
  expect_equal(auc(x = 1:10, y = (1:10)**2), 669/2)
  expect_equal(auc(x = c(1:5, NA, 6:10), y = c((1:5)**2, NA, (6:10)**2)), 669/2)
  expect_equal(auc(x = c(1:3, NA, 4:10), y = c((1:7)**2, NA, (8:10)**2)), 375)
  expect_equal(auc(x = c(1:10), y = c(1:5, 5:1)), 29)
  #Data is all NA's
  expect_equal(auc(x = 1:10, y = rep(NA, 10)), NA)
  #Data is mostly NA's
  expect_equal(auc(x = 1:10, y = c(7, rep(NA, 9))), NA)
  expect_equal(auc(x = 1:10, y = c(1, 2, rep(NA, 8))), 1.5)
})

test_that("auc returns correctly with xlim", {
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(2, 9)), 241.5)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(1, 9)), 244)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(4, 10)), 313)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(3.5, 7.5)), 127)
  expect_equal(auc(x = 1:10, y = seq(2, 20, 2), xlim = c(3.5, 7.5)), 4*(7+15)/2)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(2, NA)), 332)
  expect_equal(auc(x = 1:10, y = (1:10)**2, xlim = c(NA, 7)), 115)
  expect_error(auc(x = 1:10, y = (1:10)**2, xlim = c(NA, NA)))
  expect_warning(auc(x = 2:8, y = (2:8)**2, xlim = c(1, 8)),
                 "xlim specifies lower limit below the range of x")
  expect_warning(auc(x = 2:8, y = (2:8)**2, xlim = c(2, 9)),
                 "xlim specifies upper limit above the range of x")
  expect_equal(auc(x = 1:10, y = c(NA, (2:9)**2, NA), xlim = c(1, 10)), 241.5)
})

test_that("lag_time returns correctly", {
  library(dplyr)
  baranyi_func <- function(r, k, v, q0, m, d0, t_vals) {
    #Copied from Ram et al 2019
    if (anyNA(c(r, k, v, q0, m, d0, t_vals))) {return(NA)}
    if (q0 < 0) {q0 <- 0}
    a <- t_vals + 1/m*log((exp(-m*t_vals)+q0)/(1+q0))
    d <- k/(1-(1-((k/d0)**v))*exp(-r*v*a))**(1/v)
    return(d)
  }
  dat <- data.frame(x = seq(from = 0, to = 200, by = 3),
                    y = baranyi_func(r = .1, k = 1, v = 1, q0 = .01, m = .1, 
                                     d0 = 0.001, 
                                     t_vals = seq(from = 0, to = 200, by = 3)))
  dat$grp <- "A"
  dat <- mutate(group_by(dat, grp),
                deriv = calc_deriv(y = y, x = x, percapita = TRUE, 
                                blank = 0, trans_y = "log", window_width_n = 5))
  y0 <- min(dat$y, na.rm = TRUE)
  y1 <- dat$y[which.max(dat$deriv)]
  x1 <- dat$x[which.max(dat$deriv)]
  m <- max(dat$deriv, na.rm = TRUE)
  lag <- lag_time(slope = m, y0 = y0, x1 = x1, y1 = y1)
  expect_equal(log(y1) - m*x1 + m*lag, log(y0))
  if(F) {
    plot(dat$x, log(dat$y))
    points(x1, log(y1), col = "red")
    abline(a = log(y1) - m*x1, b = m)
    abline(h = log(y0), lty = 2)
    abline(v = lag, lty = 2)
    plot(dat$x, dat$deriv)
    abline(h = m)
  }
  
  #y data has lots of NA's
  dat <- data.frame(x = 1:100, y = (-10):89, grp = rep("A", 100))
  dat <- mutate(group_by(dat, grp), 
                deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))
  expect_warning(expect_warning(
    lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
    "NaNs produced"),
    "infinite values")
  expect_equal(lag, 12)
  
  #y data is nearly all NAs
  dat <- data.frame(x = 1:10, y = c(1, rep(NA, 8), 100), grp = rep("A", 10))
  dat <- mutate(group_by(dat, grp), 
                deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))
  expect_equal(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
               1)
  dat <- data.frame(x = 1:10, y = c(1, rep(NA, 7), 81, 100), grp = rep("A", 10))
  dat <- mutate(group_by(dat, grp), 
                deriv = calc_deriv(x = x, y = y, percapita = TRUE, blank = 0))
  expect_equal(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
               1)
  
  #y data is all NAs
  dat <- data.frame(x = 1:10, y = rep(NA, 10), deriv = 1:10)
  expect_equal(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
               as.numeric(NA))
  
  #All y values are 0
  dat <- data.frame(x = 1:10, y = rep(0, 10), deriv = 1:10)
  expect_warning(lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 "infinite values created")
  expect_equal(lag, as.numeric(NA))
  
  #All deriv are 0
  dat <- data.frame(x = 1:10, y = 1:10, deriv = rep(0, 10))
  expect_warning(lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 "multiple timepoints have")
  expect_equal(lag, NaN)
  
  #All deriv are the same (so matches the first one with warning)
  dat <- data.frame(x = 1:10, y = 1:10, deriv = rep(5, 10))
  expect_warning(lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 "multiple timepoints have")
  expect_equal(lag, 1)
  
  #Deriv has NA values where min(y) is
  dat <- data.frame(x = 1:10, y = exp(1:10), deriv = c(NA, 1, rep(0.9, 8)))
  expect_equal(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv), 1)
  
  #min(y) occurs where x is NA
  dat <- data.frame(x = c(NA, 2:10), y = exp(1:10), deriv = 1:10)
  expect_warning(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 regexp = "min\\(y\\) does not equal min")
  
  #lag time is less than min(x)
  dat <- data.frame(x = 1:10, y = exp(1:10), deriv = c(NA, 0.5, rep(0.1, 8)))
  expect_warning(lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 regexp = "indicating no identifiable lag phase")
  expect_equal(lag, 0)
  
  #lag time is less than min(x[!is.na(y)])
  dat <- data.frame(x = 1:10, y = c(NA, exp(2:10)), 
                    deriv = c(NA, NA, 0.8, rep(0.1, 7)))
  expect_warning(lag <- lag_time(x = dat$x, y = dat$y, deriv = dat$deriv),
                 regexp = "indicating no identifiable lag phase")
  expect_equal(lag, 1.75)
  
  #na.rm = FALSE
  dat <- data.frame(x = 1:10, y = c(NA, exp(2:10)), 
                    deriv = c(NA, NA, 0.8, rep(0.1, 7)))
  expect_equal(lag_time(x = dat$x, y = dat$y, deriv = dat$deriv, na.rm = FALSE),
               as.numeric(NA))
})

test_that("doubling_time returns correctly", {
  yvals <- log(2)/c(5, 10, 20, 30, 60) #minutes
  
  expect_equal(doubling_time(y = yvals),
               c(5, 10, 20, 30, 60))
  
  yvals <- log(2)/(60*c(5, 10, 20, 30, 60)) #seconds
  expect_equal(doubling_time(y = yvals, x_scale = 60),
               c(5, 10, 20, 30, 60))
  
  yvals <- c(log(2)/c(-5, 10, NA, 30), 0)
  expect_equal(doubling_time(y = yvals),
               c(-5, 10, NA, 30, Inf))
})

test_that("first_maxima matches find_local_extrema results", {
  expect_equal(
    first_maxima(y = (20 - abs(12 - 1:20))),
    find_local_extrema(y = (20 - abs(12 - 1:20)),
                       return_minima = FALSE,
                       return_endpoints = FALSE,
                       window_width_n = 3))
  expect_equal(
    first_maxima(y = (20 - abs(12 - 1:20)), return = "y"), 20)
  expect_equal(
    first_maxima(x = 21:40, y = (20 - abs(12 - 1:20)), return = "x"), 32)
})

test_that("first_maxima and first_minima work", {
  expect_no_error(lapply(sapply(5:100, FUN = function(x) {return(1:x)}),
         FUN = function(x) {first_maxima(y = x)}))
  expect_no_warning(lapply(sapply(5:100, FUN = function(x) {return(1:x)}),
                         FUN = function(x) {first_maxima(y = x)}))
  expect_no_error(lapply(sapply(5:100, FUN = function(x) {return(1:x)}),
                         FUN = function(x) {first_minima(y = x)}))
  expect_no_warning(lapply(sapply(5:100, FUN = function(x) {return(1:x)}),
                           FUN = function(x) {first_minima(y = x)}))
})

test_that("find_local_extrema works correctly", {
  #data in order
  dat <- data.frame(x = 1:20, y = (20 - abs(12 - 1:20)) + 5*(1:20 == 2))
  
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(2, 12))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_height = 3),
               c(2, 12))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 13),
               12)
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_height = 5),
               12)
  
  #data out of order
  dat <- dat[c(14:20, 1:13), ]
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(9, 19))
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_height = 3),
               c(9, 19))
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_width_n = 13),
               19)
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_height = 5),
               19)
  
  #data with NA's
  dat$x[8] <- NA
  dat$y[20] <- NA
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(9, 19))
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_height = 3),
               c(9, 19))
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_width_n = 13),
               19)
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_height = 5),
               19)
  
  #Data that is all NA's
  expect_equal(find_local_extrema(y = rep(NA, 10), window_width_n = 3), NA)
  
  #with subset
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_width_n = 5, 
                                  subset = dat$x<10 & !is.na(dat$x)),
               c(9, 16))
  expect_equal(find_local_extrema(x = dat$x, y = dat$y, return_minima = FALSE,
                                  window_height = 3, 
                                  subset = dat$x>5 & !is.na(dat$x)),
               c(19))
  
  #data where last index is maxima
  dat <- data.frame(x = 1:20, y = (1:20)**2)
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 3),
               20)
  
  #data where there are tie values
  dat <- data.frame(x = 1:20, y = c(1:10, 10:1))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 3),
               10)
  
  #data where peaks are exactly (width_limit-1)/2 apart
  dat <- data.frame(x = 1:20, y = c(1:7, 2, 7.1, 1:11))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(9, 20))
  dat <- data.frame(x = 1:20, y = c(1:7, 2, 7, 1:11))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(7, 20))
  dat <- data.frame(x = 1:20, y = c(1:7, 2, 6.9, 1:11))
  expect_equal(find_local_extrema(y = dat$y, return_minima = FALSE,
                                  window_width_n = 5),
               c(7, 20))
})

test_that("first_below works correctly with no subset", {
  dat <- data.frame(x = 1:20,
                    y = 20 - abs(12 - 1:20))
  expect_equal(first_below(y = 20:1, threshold = 15), 7)
  expect_equal(first_below(y = 20:1, threshold = 10.5), 11)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", threshold = 10), 31)
  expect_equal(
    first_below(y = 20:1, x = 21:40, return = "x", threshold = 10.5), 30.5)
})

test_that("first_below works correctly with subset", {
  dat <- data.frame(x = 1:20,
                    y = 20 - abs(12 - 1:20))
  expect_equal(first_below(y = 20:1, threshold = 15, subset = (1:20 < 10)), 7)
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
                           subset = dat$x > 15 & !is.na(dat$x)),
    c(3))
  expect_equal(
    find_threshold_crosses(y = dat$y, x = dat$x,
                           return_rising = FALSE, return_falling = TRUE,
                           return_endpoints = TRUE,
                           threshold = 16.5, return = "index",
                           subset = dat$x < 25 & !is.na(dat$x)),
    c(15, 30))
  
  #data where it never crosses
  expect_equal(find_threshold_crosses(y = 1:10, return_falling = FALSE, 
                                      threshold = 11),
               NA)
  
  #data where startpoint should be returned
  expect_equal(find_threshold_crosses(y = 10:20, threshold = 9), 1)
  expect_equal(find_threshold_crosses(y = 1:10, threshold = 10), 1)
  expect_equal(find_threshold_crosses(y = c(1:5, 1:5), threshold = 3), 
               c(1,4, 6, 9))
  expect_equal(find_threshold_crosses(x = 2:12, y = 10:20, 
                                      threshold = 9, return = "x"), 2)
  expect_equal(find_threshold_crosses(x = 2:11, y = 1:10, 
                                      threshold = 10, return = "x"), 2)
  expect_equal(find_threshold_crosses(x = 2:11, y = c(1:5, 1:5), 
                                      threshold = 3, return = "x"), 
               c(2, 4, 6.5, 9))
  
  #data where all values are NA
  expect_equal(find_threshold_crosses(y = rep(NA, 5), threshold = 5), NA)
  
  #data where nearly all values are NA
  expect_equal(find_threshold_crosses(y = c(NA, NA, 3, NA, NA), threshold = 5),
               3)
  expect_equal(find_threshold_crosses(y = c(NA, NA, 3, NA, NA), threshold = 5,
                                      return_falling = FALSE),
               NA)
  
  #data where startpoint of subset should be returned
  expect_equal(find_threshold_crosses(
    y = c(1:5, 5:1), subset = c(rep(F, 3), rep(T, 7)), threshold = 2.5), c(4, 9))
  expect_equal(find_threshold_crosses(
    y = c(5:1, 1:5), subset = c(rep(F, 3), rep(T, 7)), threshold = 3.5), c(4, 9))
})

