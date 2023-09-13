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
  library(dplyr)
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "grp" = rep("A", 100))
  manual_expect_win5 <- c(NA, NA, rep(0, (nrow(data)-4)), NA, NA)
  for (i in 3:98) {manual_expect_win5[i] <- mean(data$dens[(i-2):(i+2)])}
  temp <- mutate(group_by(data, grp),
                        sm = smooth_data(x = time, y = dens,
                           sm_method = "moving-average",
                           window_width_n = 5))
  expect_equal(temp$sm, expected = manual_expect_win5)
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
  library(dplyr)
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "grp" = rep("A", 100))
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = time, y = dens,
                sm_method = "loess", span = 0.5))
  expect_equal(temp$sm, 
               expected = loess(dens ~ time, data, span = 0.5, 
                     na.action = "na.exclude")$fitted)
  
  #Now test with out-of-order data
  data2 <- data.frame(x = c(50:100, 1:49), y = sqrt(c(50:100, 1:49)),
                      grp = rep("A", 100))
  temp <- mutate(group_by(data2, grp),
                 sm = smooth_data(x = x, y = y,
                                  sm_method = "loess", span = 0.5))
  expect_equal(temp$sm,
               expected = loess(y ~ x, data2, span = 0.5,
                                na.action = "na.exclude")$fitted)
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(50:65, NA, 66:100, NA, 2:48, NA), 
                      y = c(NA, sqrt(50:75), NA, sqrt(76:100), sqrt(1:48)),
                      grp = rep("A", 101))
  expected3 <- predict(loess(y ~ x, data3, span = 0.5, na.action = "na.exclude"),
                       data3)
  names(expected3) <- NULL
  temp <- mutate(group_by(data3, grp),
                 sm = smooth_data(x = data3$x, y = data3$y,
                                  sm_method = "loess", span = 0.5))
  expect_equal(temp$sm, expected = expected3)
})

test_that("smooth_data returns properly for gam", {
  library(mgcv)
  library(dplyr)
  set.seed(1)
  data <- data.frame("time" = 1:100,
                     "dens" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "grp" = rep("A", 100))
  expect1 <- predict(gam(formula = dens ~ s(time), data = data),
                     data)
  names(expect1) <- NULL
  dim(expect1) <- NULL
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = time, y = dens, 
                                  sm_method = "gam"))
  expect_equal(temp$sm, expected = expect1)

  data2 <- data.frame(x = c(50:100, 1:49), y = sqrt(c(50:100, 1:49)),
                      grp = rep("A", 100))
  expect2 <- predict(gam(formula = y ~ s(x), data = data2),
                     data2)
  names(expect2) <- NULL
  dim(expect2) <- NULL
  temp <- mutate(group_by(data2, grp),
                 sm = smooth_data(x = data2$x, y = data2$y, sm_method = "gam"))
  expect_equal(temp$sm, expected = expect2)
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(50:65, NA, 66:100, NA, 2:48, NA), 
                      y = c(NA, sqrt(50:75), NA, sqrt(76:100), sqrt(1:48)),
                      grp = rep("A", 101))
  expect3 <- predict(gam(formula = y ~ s(x), data = data3),
                     data3)
  names(expect3) <- NULL
  dim(expect3) <- NULL
  temp <- mutate(group_by(data3, grp),
                 sm = smooth_data(x = x, y = y, sm_method = "gam"))
  expect_equal(temp$sm, expected = expect3)
  
  #Now test when passing arguments for s() (e.g. k)
  expect4 <- predict(gam(formula = dens ~ s(time, k = 5), data = data),
                     data)
  names(expect4) <- NULL
  dim(expect4) <- NULL
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = time, y = dens, 
                                  sm_method = "gam", k = 5))
  expect_equal(temp$sm, expected = expect4)
  
  #& non-k args to s
  expect5 <-
    predict(gam(formula = dens ~ s(time, k = 5, bs = "cr"), data = data),
                     data)
  names(expect5) <- NULL
  dim(expect5) <- NULL
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = time, y = dens, sm_method = "gam",
                                  k = 5, bs = "cr"))
  expect_equal(temp$sm, expected = expect5)

  #& mix of args to s() and to gam()
  expect6 <-
    predict(gam(formula = dens ~ s(time, k = 5, bs = "cr"),
                data = data, subset = 1:90),
            data)
  names(expect6) <- NULL
  dim(expect6) <- NULL
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = time, y = dens, sm_method = "gam",
                                  k = 5, bs = "cr", subset = 1:90))
  expect_equal(temp$sm, expected = expect6)
})

test_that("smooth_data checks for grouping", {
  library(dplyr)
  mydf <- data.frame(x = 1:20, y = sqrt(1:20), 
                     grp = rep(c("A", "B"), each = 10))
  
  expect_warning(smooth_data(window_width_n = 5, x = mydf$x, y = mydf$y,
                             sm_method = 'moving-median'),
                 ".* called outside of dplyr::mutate and subset_by = NULL")
  expect_warning(mutate(mydf,
                        sm = smooth_data(window_width_n = 5, x = x, y = y, 
                                         sm_method = 'moving-median')),
                 ".* called on an ungrouped data.frame and subset_by = NULL")
  expect_no_warning(mutate(group_by(mydf, grp),
                           sm = smooth_data(window_width_n = 5, x = x, y = y, 
                                            sm_method = 'moving-median')))
})
  