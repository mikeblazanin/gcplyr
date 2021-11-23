context("Processing functions")
library(testthat)
library(growth.curves.pkg)

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
})
