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

test_that("smooth_data returns properly for smooth.spline", {
  add_nas <- function(x, nas_indices_removed) {
      return_indices <- 1:length(x)
      for (index in nas_indices_removed) {
        return_indices[return_indices >= index] <-
          return_indices[return_indices >= index] + 1
      }
      
      out <- rep(NA, length(x) + length(nas_indices_removed))
      out[return_indices] <- x
      
      return(out)
  }
  
  library(dplyr)
  set.seed(1)
  data <- data.frame("x" = 1:100,
                     "y" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "grp" = rep("A", 100))
  expect1 <- stats::smooth.spline(x = data$x, y = data$y)$y
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = x, y = y, 
                                  sm_method = "smooth.spline"))
  expect_equal(temp$sm, expected = expect1)
  
  #Out of order data
  data2 <- data.frame(x = c(50:100, 1:49), y = sqrt(c(50:100, 1:49)),
                      grp = rep("A", 100))
  expect2 <- stats::smooth.spline(x = data2$x, y = data2$y)
  expect2$y <- expect2$y[match(data2$x, expect2$x)]
  temp <- mutate(group_by(data2, grp),
                 sm = smooth_data(x = x, y = y, sm_method = "smooth.spline"))
  expect_equal(temp$sm, expected = as.vector(expect2$y))
  
  #Now test with NA's and out-of-order data
  data3 <- data.frame(x = c(50:65, NA, 66:100, NA, 2:48, NA), 
                      y = c(NA, sqrt(50:75), NA, sqrt(76:100), sqrt(1:48)),
                      grp = rep("A", 101))
  expect3 <- 
    stats::smooth.spline(x = data3$x[!is.na(data3$x) & !is.na(data3$y)], 
                         y = data3$y[!is.na(data3$x) & !is.na(data3$y)])
  expect3$y <- expect3$y[match(data3$x, expect3$x)]
  temp <- mutate(group_by(data3, grp),
                 sm = smooth_data(x = x, y = y, sm_method = "smooth.spline"))
  expect_equal(temp$sm, expected = expect3$y)
  
  #Now test with duplicate x values
  data4 <- data.frame(x = c(50:100, 1:49, 50), y = sqrt(c(50:100, 1:49, 51)),
                      grp = rep("A", 101))
  expect4 <- 
    stats::smooth.spline(x = data4$x, y = data4$y)
  expect4$y <- expect4$y[match(data4$x, expect4$x)]
  temp <- mutate(group_by(data4, grp),
                 sm = smooth_data(x = x, y = y, sm_method = "smooth.spline"))
  expect_equal(temp$sm, expected = expect4$y)
  
  #Now test when passing arguments
  expect5 <- stats::smooth.spline(x = data$x, y = data$y, df = 7)$y
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = x, y = y, 
                                  sm_method = "smooth.spline", df = 7))
  expect_equal(temp$sm, expected = expect5)
  
  expect5 <- stats::smooth.spline(x = data$x, y = data$y, spar = 0.5)$y
  temp <- mutate(group_by(data, grp),
                 sm = smooth_data(x = x, y = y, 
                                  sm_method = "smooth.spline", spar = 0.5))
  expect_equal(temp$sm, expected = expect5)
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
  
test_that("interpolate_prediction works correctly", {
  x <- 1:10
  y <- x**2
  
  expect_equal(interpolate_prediction(x = x, y = y, newdata = c(2.5, 6.5)),
               c(6.5, 42.5))
  expect_equal(interpolate_prediction(x = x, y = y, newdata = c(3, 5, 7)),
               c(3, 5, 7)**2)
  expect_equal(interpolate_prediction(x = x, y = y, newdata = 0,
                                      extrapolate_predictions = FALSE),
               as.numeric(NA))
  expect_equal(interpolate_prediction(x = x, y = y, newdata = 15,
                                      extrapolate_predictions = FALSE),
               as.numeric(NA))
  expect_equal(interpolate_prediction(x = x, y = y, newdata = c(2.5, 5, 15),
                                      extrapolate_predictions = FALSE),
               c(6.5, 25, as.numeric(NA)))
  expect_equal(interpolate_prediction(x = x, y = y, newdata = c(0, 11)),
               c(-2, 119))
  expect_equal(interpolate_prediction(x = x, y = y, newdata = c(0, 0.5, 10.5, 11)),
               c(-2, -0.5, 109.5, 119))
})

test_that("train_smooth_data and train + makemethod_train_smooth_data match", {
  library(dplyr)
  library(caret)
  dat <- trans_wide_to_tidy(example_widedata, id_cols = "Time")

  #Default conditions
  for (mymethod in c("moving-average", "moving-median", "loess",
                     "gam", "smooth.spline")) {
    expect_equal(
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = train_smooth_data(
                x = Time, y = Measurements,
                sm_method = mymethod,
                trControl = caret::trainControl(
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))),
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = caret::train(
                x = data.frame(x = Time), y = Measurements,
                method = makemethod_train_smooth_data(sm_method = mymethod),
                trControl = caret::trainControl(
                  method = "cv",
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))$results)
    )
  }

  #Specifying length
  for (mymethod in c("moving-average", "moving-median", "loess",
                     "gam", "smooth.spline")) {
    expect_equal(
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = train_smooth_data(
                x = Time, y = Measurements,
                sm_method = mymethod, tuneLength = 5,
                trControl = caret::trainControl(
                  seeds = c(rep(list(c(1,1,1,1,1)), 26), 1)))),
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = caret::train(
                x = data.frame(x = Time), y = Measurements,
                method = makemethod_train_smooth_data(sm_method = mymethod),
                tuneLength = 5,
                trControl = caret::trainControl(
                  method = "cv",
                  seeds = c(rep(list(c(1,1,1,1,1)), 26), 1)))$results)
    )
  }

  methods <- c("moving-average", "moving-median", "loess",
               "gam", "smooth.spline")
  tuneGrids_list <- list(list("window_width_frac" = c(0.2, 0.4)),
                         list("window_width_frac" = c(0.2, 0.4)),
                         list("span" = c(0.5, 0.6)),
                         list("k" = c(7, 9)),
                         list("spar" = c(0.5, 0.6)))
  #Specifying parameter values
  for (i in 1:length(methods)) {
    mymethod <- methods[i]
    mytuneGrid <- tuneGrids_list[[i]]
    expect_equal(
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = train_smooth_data(
                x = Time, y = Measurements,
                sm_method = mymethod,
                tuneGrid = mytuneGrid,
                trControl = caret::trainControl(
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))),
      reframe(group_by(filter(dat, Well %in% c("A1", "A7")), Well),
              train = caret::train(
                x = data.frame(x = Time), y = Measurements,
                method = makemethod_train_smooth_data(sm_method = mymethod),
                tuneGrid = as.data.frame(mytuneGrid),
                trControl = caret::trainControl(
                  method = "cv",
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))$results)
    )
  }

  #subset_by used
  for (mymethod in c("moving-average", "moving-median", "loess",
                     "gam", "smooth.spline")) {
    expect_equal(
      reframe(filter(dat, Well %in% c("A1", "A7")),
              train = train_smooth_data(
                x = Time, y = Measurements,
                sm_method = mymethod,
                subset_by = Well,
                trControl = caret::trainControl(
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))),
      reframe(filter(dat, Well %in% c("A1", "A7")),
              train = caret::train(
                x = data.frame(x = Time), y = Measurements,
                method = makemethod_train_smooth_data(sm_method = mymethod,
                                             subset_by = "Well"),
                trControl = caret::trainControl(
                  method = "cv",
                  seeds = c(rep(list(c(1,1,1)), 26), 1)))$results)
    )
  }
})
