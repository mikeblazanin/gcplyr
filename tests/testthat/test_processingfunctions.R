context("Processing functions")
library(testthat)
library(gcplyr)

#For testing smooth_data


# temp <- mgcv::gam(dens ~ s(time), data=data)
# temp <- mgcv::gam(formula = formula, data = data)
# 
# plot(test$time, test$dens)
# lines(test$time, temp$fitted)

test_that("Moving average returns correctly", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  
  expect_error(moving_average(formula = dens ~ time + treat,
                              data = data, window_width = 3))
  expect_error(moving_average(formula = ~ time,
                              data = data, window_width = 3))
  expect_error(moving_average(formula = dens ~ lime,
                              data = data, window_width = 3))
  expect_error(moving_average(formula = lens ~ time,
                              data = data, window_width = 3))
  expect_error(moving_average(formula = dens ~ time,
                              data = data, window_width = 4))
  expect_identical(moving_average(formula = dens ~ time,
                                  data = data, window_width = 1),
                   expected = data$dens)
  
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- mean(data$dens[(i-2):(i+2)])}
  expect_equal(moving_average(formula = dens ~ time,
                                  data = data, window_width = 5),
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

test_that("smooth_data returns properly for moving-average", {
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5))
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- mean(data$dens[(i-2):(i+2)])}
  expect_equal(smooth_data(x = data$time, y = data$dens,
                           method = "moving-average",
                           window_width = 5),
               expected = manual_expect_win5)
  data2 <- data.frame("time" = c(1:100, 1:100),
                      "dens" = c(data$dens, data$dens + 10),
                      "treat" = rep(c("A", "B"), each = 100))
  expect_equal(smooth_data(x = data2$time, y = data2$dens,
                           method = "moving-average",
                           subset_by = data2$treat,
                           window_width = 5),
               expected = c(manual_expect_win5, manual_expect_win5 + 10))
})

test_that("moving_median returns as expected", {
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  expected <- c(NA, NA, rep(0, 96), NA, NA)
  for (i in 3:98) {expected[i] <- median(data$dens[(i-2):(i+2)])}
  expect_equal(moving_median(dens ~ time, data, window_width = 5),
               expected)
  
  expected2 <- c(NA, rep(0, 98), NA)
  for (i in 2:99) {expected2[i] <- median(data$dens[(i-1):(i+1)])}
  expect_equal(moving_median(dens ~ time, data, window_width = 3),
               expected2)
})