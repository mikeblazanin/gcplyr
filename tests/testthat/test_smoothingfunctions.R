library(testthat)
library(gcplyr)

test_that("Moving average returns correctly", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  
  expect_error(moving_average(formula = dens ~ time + treat,
                              data = data, window_width_n = 3))
  expect_error(moving_average(formula = ~ time,
                              data = data, window_width_n = 3))
  expect_error(moving_average(formula = dens ~ lime,
                              data = data, window_width_n = 3))
  expect_error(moving_average(formula = lens ~ time,
                              data = data, window_width_n = 3))
  expect_error(moving_average(formula = dens ~ time,
                              data = data, window_width_n = 4))
  expect_identical(moving_average(formula = dens ~ time,
                                  data = data, window_width = 0),
                   expected = data$dens)
  expect_identical(moving_average(formula = dens ~ time,
                                  data = data, window_width_n = 1),
                   expected = data$dens)
  
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- mean(data$dens[(i-2):(i+2)])}
  expect_equal(moving_average(formula = dens ~ time,
                                  data = data, window_width_n = 5),
                   expected = manual_expect_win5)
  
  #Now test with out-of-order data
  data2 <- data.frame(x = c(6:9, 1:5), y = c(6:9, 1:5)**2)
  expect_equal(moving_average(y ~ x, data2, window_width_n = 3),
               expected = c((25+36+49)/3, (36+49+64)/3, (49+64+81)/3,
                            NA, NA, (1+4+9)/3, (4+9+16)/3, (9+16+25)/3,
                            (16+25+36)/3))
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(6:9,NA, 1:5), 
                      y = c(NA, c(7:9)**2, 50, c(1:5)**2))
  expect_equal(moving_average(y ~ x, data3, window_width_n = 3),
               expected = c(NA, (25+49+64)/3, (49+64+81)/3,
                            NA, NA, NA, (1+4+9)/3,
                            (4+9+16)/3, (9+16+25)/3, (16+25+49)/3))
})

test_that("Moving median returns correctly", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  
  expect_error(moving_median(formula = dens ~ time + treat,
                              data = data, window_width_n = 3))
  expect_error(moving_median(formula = ~ time,
                              data = data, window_width_n = 3))
  expect_error(moving_median(formula = dens ~ lime,
                              data = data, window_width_n = 3))
  expect_error(moving_median(formula = lens ~ time,
                              data = data, window_width_n = 3))
  expect_error(moving_median(formula = dens ~ time,
                              data = data, window_width_n = 4))
  expect_identical(moving_median(formula = dens ~ time,
                                  data = data, window_width_n = 1),
                   expected = data$dens)
  
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- median(data$dens[(i-2):(i+2)])}
  expect_equal(moving_median(formula = dens ~ time,
                              data = data, window_width_n = 5),
               expected = manual_expect_win5)
  
  #Now test with out-of-order data
  data2 <- data.frame(x = c(6:9, 1:5), y = c(6:9, 1:5)**2)
  expect_equal(moving_median(y ~ x, data2, window_width_n = 3),
               expected = c(36, 49, 64, NA, NA, 4, 9, 16, 25))
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(6:9,NA, 1:5), 
                      y = c(NA, c(7:9)**2, 50, c(1:5)**2))
  expect_equal(moving_median(y ~ x, data3, window_width_n = 3),
               expected = c(NA, 49, 64, NA, NA, NA, 4, 9, 16, 25))
})

test_that("smooth_data returns properly for moving-average", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5))
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- mean(data$dens[(i-2):(i+2)])}
  expect_equal(smooth_data(x = data$time, y = data$dens,
                           sm_method = "moving-average",
                           window_width_n = 5),
               expected = manual_expect_win5)
  data2 <- data.frame("time" = c(1:100, 1:100),
                      "dens" = c(data$dens, data$dens + 10),
                      "treat" = rep(c("A", "B"), each = 100))
  expect_equal(smooth_data(x = data2$time, y = data2$dens,
                           sm_method = "moving-average",
                           subset_by = data2$treat,
                           window_width_n = 5),
               expected = c(manual_expect_win5, manual_expect_win5 + 10))
})

test_that("smooth_data returns properly for loess", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5))
  expect_equal(
    smooth_data(x = data$time, y = data$dens,
                sm_method = "loess", span = 0.5),
    expected = loess(dens ~ time, data, span = 0.5, 
                     na.action = "na.exclude")$fitted)
  
  #Now test with out-of-order data
  data2 <- data.frame(x = c(50:100, 1:49), y = sqrt(c(50:100, 1:49)))
  expect_equal(smooth_data(x = data2$x, y = data2$y,
                           sm_method = "loess", span = 0.5),
               expected = loess(y ~ x, data2, span = 0.5,
                                na.action = "na.exclude")$fitted)
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(50:65, NA, 66:100, NA, 2:48, NA), 
                      y = c(NA, sqrt(50:75), NA, sqrt(76:100), sqrt(1:48)))
  expected3 <- predict(loess(y ~ x, data3, span = 0.5, na.action = "na.exclude"),
                       data3)
  names(expected3) <- NULL
  expect_equal(smooth_data(x = data3$x, y = data3$y,
                           sm_method = "loess", span = 0.5),
               expected = expected3)
})

test_that("smooth_data returns properly for gam", {
  library(mgcv)
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5))
  expect1 <- predict(gam(formula = dens ~ s(time), data = data),
                     data)
  names(expect1) <- NULL
  dim(expect1) <- NULL
  expect_equal(
    smooth_data(x = data$time, y = data$dens, sm_method = "gam"),
    expected = expect1)

  data2 <- data.frame(x = c(50:100, 1:49), y = sqrt(c(50:100, 1:49)))
  expect2 <- predict(gam(formula = y ~ s(x), data = data2),
                     data2)
  names(expect2) <- NULL
  dim(expect2) <- NULL
  expect_equal(smooth_data(x = data2$x, y = data2$y, sm_method = "gam"),
               expected = expect2)
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(50:65, NA, 66:100, NA, 2:48, NA), 
                      y = c(NA, sqrt(50:75), NA, sqrt(76:100), sqrt(1:48)))
  expect3 <- predict(gam(formula = y ~ s(x), data = data3),
                     data3)
  names(expect3) <- NULL
  dim(expect3) <- NULL
  expect_equal(smooth_data(x = data3$x, y = data3$y, sm_method = "gam"),
               expected = expect3)
  
  #Now test when passing arguments for s() (e.g. k)
  expect4 <- predict(gam(formula = dens ~ s(time, k = 5), data = data),
                     data)
  names(expect4) <- NULL
  dim(expect4) <- NULL
  expect_equal(
    smooth_data(x = data$time, y = data$dens, sm_method = "gam", k = 5),
    expected = expect4)
  
  #& non-k args to s
  expect5 <-
    predict(gam(formula = dens ~ s(time, k = 5, bs = "cr"), data = data),
                     data)
  names(expect5) <- NULL
  dim(expect5) <- NULL
  expect_equal(
    smooth_data(x = data$time, y = data$dens, sm_method = "gam",
                k = 5, bs = "cr"),
    expected = expect5)

  #& mix of args to s() and to gam()
  expect6 <-
    predict(gam(formula = dens ~ s(time, k = 5, bs = "cr"),
                data = data, subset = 1:90),
            data)
  names(expect6) <- NULL
  dim(expect6) <- NULL
  expect_equal(
    smooth_data(x = data$time, y = data$dens, sm_method = "gam",
                k = 5, bs = "cr", subset = 1:90),
    expected = expect6)
})

test_that("smooth_data checks for grouping", {
  library(dplyr)
  if(FALSE) {#to be updated when stackoverflow thread is updated
  expect_warning(smooth_data(window_width_n = 5, x = mtcars$cyl, y = mtcars$mpg,
                             sm_method = 'moving-median'))
  expect_warning(mutate(mtcars,
                        sm = smooth_data(window_width_n = 5, x = cyl, y = mpg, 
                                         sm_method = 'moving-median')))
  }
  
})