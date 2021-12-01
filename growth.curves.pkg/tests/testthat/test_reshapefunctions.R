context("Reshaping functions")
library(testthat)
library(growth.curves.pkg)

test_that("Pivot_longer works on single dataframe", {
  data <- data.frame("time" = 1:100,
                     "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5),
                     "Pop2" = 20/(1+exp(-.1*((1:100) - 50))) + 
                       rnorm(100, sd = 0.5))
  data_lng <- pivot_wide_longer(data,
                    data_cols = c("Pop1", "Pop2"))
  data_lng <- data_lng[order(data_lng$Well, data_lng$time), ]
  row.names(data_lng) <- 1:200
  expect_equal(data_lng,
               expected = data.frame(time = c(1:100, 1:100),
                                     Well = c(rep("Pop1", 100), rep("Pop2", 100)),
                                     Measurements = c(data$Pop1, data$Pop2)))
})

test_that("Pivot_longer works on list of dataframes", {
  data_lst <- list("df1" = data.frame("time" = 1:100,
                     "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5),
                     "Pop2" = 20/(1+exp(-.1*((1:100) - 50))) +
                       rnorm(100, sd = 0.5)),
                   "df2" = data.frame("time" = 1:100,
                              "Pop3" = 10/(1+exp(-.1*((1:100) - 50))) +
                                rnorm(100, sd = 0.5),
                              "Pop4" = 20/(1+exp(-.1*((1:100) - 50))) +
                                rnorm(100, sd = 0.5)))
  data_lst_lng <- pivot_wide_longer(data_lst,
                                id_cols = "time")
  for (i in 1:length(data_lst_lng)) {
    data_lst_lng[[i]] <- data_lst_lng[[i]][order(data_lst_lng[[i]]$Well, 
                                                 data_lst_lng[[i]]$time), ]
    row.names(data_lst_lng[[i]]) <- 1:nrow(data_lst_lng[[i]])
  }
  expect_equal(data_lst_lng,
               expected = 
                 list(
                   "df1" = data.frame(time = c(1:100, 1:100),
                                     Well = c(rep("Pop1", 100), rep("Pop2", 100)),
                                     Measurements = c(data_lst[[1]]$Pop1, 
                                                      data_lst[[1]]$Pop2)),
                   "df2" = data.frame(time = c(1:100, 1:100),
                              Well = c(rep("Pop3", 100), rep("Pop4", 100)),
                              Measurements = c(data_lst[[2]]$Pop3, 
                                               data_lst[[2]]$Pop4))))
})

test_that("Merge function collapses lists of dfs correctly", {
  data_lst <- list("df1" = data.frame("time" = 1:100,
                                      "Pop1" = 10/(1+exp(-.1*((1:100) - 50))) +
                                        rnorm(100, sd = 0.5)),
                   "df2" = data.frame("time" = 1:100,
                                      "Pop1" = 20/(1+exp(-.1*((1:100) - 50))) +
                                        rnorm(100, sd = 0.5)))
  expect_equal(merge_dataframes(data_lst, collapse = TRUE),
               data.frame(time = c(1:100, 1:100),
                          Pop1 = c(data_lst[[1]]$Pop1, data_lst[[2]]$Pop1),
                          run = rep(c("df1", "df2"), each = 100)))
  
  data_lst2 <- list("df1" = data.frame("time" = 1:100,
                                      "fake_treat" = LETTERS[1:100]),
                   "df2" = data.frame("time" = 1:100,
                                      "fake_treat" = letters[1:100]))
  expect_equal(merge_dataframes(data_lst, data_lst2, collapse = TRUE),
               data.frame(time = c(1:100, 1:100),
                          Pop1 = c(data_lst[[1]]$Pop1, data_lst[[2]]$Pop1),
                          run = rep(c("df1", "df2"), each = 100),
                          fake_treat = c(LETTERS, rep(NA, 74),
                                         letters, rep(NA, 74))))
})
